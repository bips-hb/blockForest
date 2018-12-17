/*-------------------------------------------------------------------------------
 This file is part of Ranger.

 Ranger is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 Ranger is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Ranger. If not, see <http://www.gnu.org/licenses/>.

 Written by: Roman Hornung, Marvin N. Wright
 #-------------------------------------------------------------------------------*/

#include <set>
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <string>

#include "utility.h"
#include "ForestSurvival.h"
#include "Data.h"

ForestSurvival::ForestSurvival() :
    status_varID(0), response_timepointIDs(0) {
}

ForestSurvival::~ForestSurvival() {
}

void ForestSurvival::loadForest(size_t dependent_varID, size_t num_trees,
    std::vector<std::vector<std::vector<size_t>> >& forest_child_nodeIDs,
    std::vector<std::vector<size_t>>& forest_split_varIDs, std::vector<std::vector<double>>& forest_split_values,
    size_t status_varID, std::vector<std::vector<std::vector<double>> >& forest_chf,
    std::vector<double>& unique_timepoints, std::vector<bool>& is_ordered_variable) {

  this->dependent_varID = dependent_varID;
  this->status_varID = status_varID;
  this->num_trees = num_trees;
  this->unique_timepoints = unique_timepoints;
  data->setIsOrderedVariable(is_ordered_variable);

  // Create trees
  trees.reserve(num_trees);
  for (size_t i = 0; i < num_trees; ++i) {
    Tree* tree = new TreeSurvival(forest_child_nodeIDs[i], forest_split_varIDs[i], forest_split_values[i],
        forest_chf[i], &this->unique_timepoints, &response_timepointIDs);
    trees.push_back(tree);
  }

  // Create thread ranges
  equalSplit(thread_ranges, 0, num_trees - 1, num_threads);
}

void ForestSurvival::initInternal(std::string status_variable_name) {

  // Convert status variable name to ID
  if (!prediction_mode && !status_variable_name.empty()) {
    status_varID = data->getVariableID(status_variable_name);
  }

  data->addNoSplitVariable(status_varID);

  // If mtry not set, use floored square root of number of independent variables.
  if (mtry.size() == 1) {
    if (mtry[0] == 0) {
      unsigned long temp = ceil(sqrt((double) (num_variables - 2)));
      mtry[0] = std::max((unsigned long) 1, temp);
    }
  }

  // Set minimal node size
  if (min_node_size == 0) {
    min_node_size = DEFAULT_MIN_NODE_SIZE_SURVIVAL;
  }

  // Create unique timepoints
  std::set<double> unique_timepoint_set;
  for (size_t i = 0; i < num_samples; ++i) {
    unique_timepoint_set.insert(data->get(i, dependent_varID));
  }
  unique_timepoints.reserve(unique_timepoint_set.size());
  for (auto& t : unique_timepoint_set) {
    unique_timepoints.push_back(t);
  }

  // Create response_timepointIDs
  if (!prediction_mode) {
    for (size_t i = 0; i < num_samples; ++i) {
      double value = data->get(i, dependent_varID);

      // If timepoint is already in unique_timepoints, use ID. Else create a new one.
      uint timepointID = find(unique_timepoints.begin(), unique_timepoints.end(), value) - unique_timepoints.begin();
      response_timepointIDs.push_back(timepointID);
    }
  }

  // Sort data if extratrees and not memory saving mode
  if (splitrule == EXTRATREES && !memory_saving_splitting) {
    data->sort();
  }
}

void ForestSurvival::growInternal() {
  trees.reserve(num_trees);
  for (size_t i = 0; i < num_trees; ++i) {
    trees.push_back(new TreeSurvival(&unique_timepoints, status_varID, &response_timepointIDs));
  }
}

void ForestSurvival::allocatePredictMemory() {
  size_t num_prediction_samples = data->getNumRows();
  size_t num_timepoints = unique_timepoints.size();
  if (predict_all) {
    predictions = std::vector<std::vector<std::vector<double>>>(num_prediction_samples,
        std::vector<std::vector<double>>(num_timepoints, std::vector<double>(num_trees, 0)));
  } else if (prediction_type == TERMINALNODES) {
    predictions = std::vector<std::vector<std::vector<double>>>(1,
        std::vector<std::vector<double>>(num_prediction_samples, std::vector<double>(num_trees, 0)));
  } else {
    predictions = std::vector<std::vector<std::vector<double>>>(1,
        std::vector<std::vector<double>>(num_prediction_samples, std::vector<double>(num_timepoints, 0)));
  }
}

void ForestSurvival::predictInternal(size_t sample_idx) {
  // For each timepoint sum over trees
  if (predict_all) {
    for (size_t j = 0; j < unique_timepoints.size(); ++j) {
      for (size_t k = 0; k < num_trees; ++k) {
        predictions[sample_idx][j][k] = ((TreeSurvival*) trees[k])->getPrediction(sample_idx)[j];
      }
    }
  } else if (prediction_type == TERMINALNODES) {
    for (size_t k = 0; k < num_trees; ++k) {
      predictions[0][sample_idx][k] = ((TreeSurvival*) trees[k])->getPredictionTerminalNodeID(sample_idx);
    }
  } else {
    for (size_t j = 0; j < unique_timepoints.size(); ++j) {
      double sample_time_prediction = 0;
      for (size_t k = 0; k < num_trees; ++k) {
        sample_time_prediction += ((TreeSurvival*) trees[k])->getPrediction(sample_idx)[j];
      }
      predictions[0][sample_idx][j] = sample_time_prediction / num_trees;
    }
  }
}

void ForestSurvival::computePredictionErrorInternal() {

  size_t num_timepoints = unique_timepoints.size();

  // For each sample sum over trees where sample is OOB
  std::vector<size_t> samples_oob_count;
  samples_oob_count.resize(num_samples, 0);
  predictions = std::vector<std::vector<std::vector<double>>>(1,
      std::vector<std::vector<double>>(num_samples, std::vector<double>(num_timepoints, 0)));

  for (size_t tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
    for (size_t sample_idx = 0; sample_idx < trees[tree_idx]->getNumSamplesOob(); ++sample_idx) {
      size_t sampleID = trees[tree_idx]->getOobSampleIDs()[sample_idx];
      std::vector<double> tree_sample_chf = ((TreeSurvival*) trees[tree_idx])->getPrediction(sample_idx);

      for (size_t time_idx = 0; time_idx < tree_sample_chf.size(); ++time_idx) {
        predictions[0][sampleID][time_idx] += tree_sample_chf[time_idx];
      }
      ++samples_oob_count[sampleID];
    }
  }

  // Divide sample predictions by number of trees where sample is oob and compute summed chf for samples
  std::vector<double> sum_chf;
  std::vector<size_t> oob_sampleIDs;
  sum_chf.reserve(predictions[0].size());
  oob_sampleIDs.reserve(predictions[0].size());
  for (size_t i = 0; i < predictions[0].size(); ++i) {
    if (samples_oob_count[i] > 0) {
      double sum = 0;
      for (size_t j = 0; j < predictions[0][i].size(); ++j) {
        predictions[0][i][j] /= samples_oob_count[i];
        sum += predictions[0][i][j];
      }
      sum_chf.push_back(sum);
      oob_sampleIDs.push_back(i);
    }
  }

  // Use all samples which are OOB at least once
  //std::vector<size_t> temp;
  overall_prediction_error = 1 - computeConcordanceIndex(data, sum_chf, dependent_varID, status_varID, oob_sampleIDs);
}

// #nocov start
void ForestSurvival::writeOutputInternal() {
  *verbose_out << "Tree type:                         " << "Survival" << std::endl;
  *verbose_out << "Status variable name:              " << data->getVariableNames()[status_varID] << std::endl;
  *verbose_out << "Status variable ID:                " << status_varID << std::endl;
}

void ForestSurvival::writeConfusionFile() {

  // Open confusion file for writing
  std::string filename = output_prefix + ".confusion";
  std::ofstream outfile;
  outfile.open(filename, std::ios::out);
  if (!outfile.good()) {
    throw std::runtime_error("Could not write to confusion file: " + filename + ".");
  }

  // Write confusion to file
  outfile << "Overall OOB prediction error (1 - C): " << overall_prediction_error << std::endl;

  outfile.close();
  *verbose_out << "Saved prediction error to file " << filename << "." << std::endl;

}

void ForestSurvival::writePredictionFile() {

  // Open prediction file for writing
  std::string filename = output_prefix + ".prediction";
  std::ofstream outfile;
  outfile.open(filename, std::ios::out);
  if (!outfile.good()) {
    throw std::runtime_error("Could not write to prediction file: " + filename + ".");
  }

  // Write
  outfile << "Unique timepoints: " << std::endl;
  for (auto& timepoint : unique_timepoints) {
    outfile << timepoint << " ";
  }
  outfile << std::endl << std::endl;

  outfile << "Cumulative hazard function, one row per sample: " << std::endl;
  if (predict_all) {
    for (size_t k = 0; k < num_trees; ++k) {
      outfile << "Tree " << k << ":" << std::endl;
      for (size_t i = 0; i < predictions.size(); ++i) {
        for (size_t j = 0; j < predictions[i].size(); ++j) {
          outfile << predictions[i][j][k] << " ";
        }
        outfile << std::endl;
      }
      outfile << std::endl;
    }
  } else {
    for (size_t i = 0; i < predictions.size(); ++i) {
      for (size_t j = 0; j < predictions[i].size(); ++j) {
        for (size_t k = 0; k < predictions[i][j].size(); ++k) {
          outfile << predictions[i][j][k] << " ";
        }
        outfile << std::endl;
      }
    }
  }

  *verbose_out << "Saved predictions to file " << filename << "." << std::endl;
}

void ForestSurvival::saveToFileInternal(std::ofstream& outfile) {

  // Write num_variables
  outfile.write((char*) &num_variables, sizeof(num_variables));

  // Write treetype
  TreeType treetype = TREE_SURVIVAL;
  outfile.write((char*) &treetype, sizeof(treetype));

  // Write status_varID
  outfile.write((char*) &status_varID, sizeof(status_varID));

  // Write unique timepoints
  saveVector1D(unique_timepoints, outfile);
}

void ForestSurvival::loadFromFileInternal(std::ifstream& infile) {

  // Read number of variables
  size_t num_variables_saved;
  infile.read((char*) &num_variables_saved, sizeof(num_variables_saved));

  // Read treetype
  TreeType treetype;
  infile.read((char*) &treetype, sizeof(treetype));
  if (treetype != TREE_SURVIVAL) {
    throw std::runtime_error("Wrong treetype. Loaded file is not a survival forest.");
  }

  // Read status_varID
  infile.read((char*) &status_varID, sizeof(status_varID));

  // Read unique timepoints
  unique_timepoints.clear();
  readVector1D(unique_timepoints, infile);

  for (size_t i = 0; i < num_trees; ++i) {

    // Read data
    std::vector<std::vector<size_t>> child_nodeIDs;
    readVector2D(child_nodeIDs, infile);
    std::vector<size_t> split_varIDs;
    readVector1D(split_varIDs, infile);
    std::vector<double> split_values;
    readVector1D(split_values, infile);

    // Read chf
    std::vector<size_t> terminal_nodes;
    readVector1D(terminal_nodes, infile);
    std::vector<std::vector<double>> chf_vector;
    readVector2D(chf_vector, infile);

    // Convert chf to vector with empty elements for non-terminal nodes
    std::vector<std::vector<double>> chf;
    chf.resize(child_nodeIDs[0].size(), std::vector<double>());
//    for (size_t i = 0; i < child_nodeIDs.size(); ++i) {
//      chf.push_back(std::vector<double>());
//    }
    for (size_t j = 0; j < terminal_nodes.size(); ++j) {
      chf[terminal_nodes[j]] = chf_vector[j];
    }

    // If dependent variable not in test data, change variable IDs accordingly
    if (num_variables_saved > num_variables) {
      for (auto& varID : split_varIDs) {
        if (varID >= dependent_varID) {
          --varID;
        }
      }
    }
    if (num_variables_saved > num_variables + 1) {
      for (auto& varID : split_varIDs) {
        if (varID >= status_varID) {
          --varID;
        }
      }
    }

    // Create tree
    Tree* tree = new TreeSurvival(child_nodeIDs, split_varIDs, split_values, chf, &unique_timepoints,
        &response_timepointIDs);
    trees.push_back(tree);
  }
}
// #nocov end


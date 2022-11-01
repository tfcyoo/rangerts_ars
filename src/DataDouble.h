/*-------------------------------------------------------------------------------
 This file is part of rangerts.

 Copyright (c) [2014-2018] [Marvin N. Wright]

 This software may be modified and distributed under the terms of the MIT license.

 Please note that the C++ core of rangerts is distributed under MIT license and the
 R package "rangerts" under GPL3 license.
 #-------------------------------------------------------------------------------*/

// Ignore in coverage report (not used in R package)
// #nocov start
#ifndef DATADOUBLE_H_
#define DATADOUBLE_H_

#include <vector>
#include <utility>

#include "globals.h"
#include "utility.h"
#include "Data.h"

namespace rangertsModified {

class DataDouble: public Data {
public:
  DataDouble() = default;

  //DataDouble(const DataDouble&) = delete;
  //DataDouble& operator=(const DataDouble&) = delete;

  virtual ~DataDouble() override = default;
  
  virtual std::unique_ptr<Data> clone() const override{
    return make_unique<DataDouble>(*this);
  }

  double get_x(size_t row, size_t col) const override {
    // Use permuted data for corrected impurity importance
    size_t col_permuted = col;
    if (col >= num_cols) {
      col = getUnpermutedVarID(col);
      row = getPermutedSampleID(row);
    }

    if (col < num_cols_no_snp) {
      return x[col * num_rows + row];
    } else {
      return getSnp(row, col, col_permuted);
    }
  }

  double get_y(size_t row, size_t col) const override {
    return y[col * num_rows + row];
  }

  void reserveMemory(size_t y_cols) override {
    x.resize(num_cols * num_rows);
    y.resize(y_cols * num_rows);
  }

  void set_x(size_t col, size_t row, double value, bool& error) override {
    x[col * num_rows + row] = value;
  }

  void set_y(size_t col, size_t row, double value, bool& error) override {
    y[col * num_rows + row] = value;
  }
  

private:
  std::vector<double> x;
  std::vector<double> y;
};

} // namespace rangerts_modified

#endif /* DATADOUBLE_H_ */
// #nocov end

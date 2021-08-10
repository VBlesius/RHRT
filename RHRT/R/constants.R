###-----------------------------------------------------------------------------
# Part of RHRT: R package to assess Heart Rate Turbulence from RR interval data 
# Copyright (C) 2021 Valeria Blesius

# RHRT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2 only.

# RHRT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with RHRT.  If not, see <https://www.gnu.org/licenses/>.
###-----------------------------------------------------------------------------

c_numPreRRs <- 5 # number of regular RR-intervals before the coupling interval
c_numPostRRs <- 15 # number of regular RR-intervals after the coupling interval
c_normIL <- 800
COTO <- 0
COTS <- 2.5
COTT <- 10

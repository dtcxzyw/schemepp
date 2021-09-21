// SPDX-License-Identifier: MIT

#pragma once

namespace schemepp {
    class Scope;

    // Base Library
    // + - * /
    void initializeBuiltinArithmeticProcedure(Scope& scope);
    // real? number?
    void initializeBuiltinRTTIProcedure(Scope& scope);
    // vector-length
    void initializeBuiltinContainerProcedure(Scope& scope);
    // string<?
    void initializeBuiltinStringProcedure(Scope& scope);
    // File Library
    // Load Library
    // Process-Context Library
    // Read Library
    void initializeBuiltinReadProcedure(Scope& scope);
    // Case-Lambda Library
    // Char Library
    // Complex Library
    // CxR Library
    // Eval Library
    // Inexact Library
    // Lazy Library
    // Repl Library
    // Time Library
    // Write Library
    void initializeBuiltinWriteProcedure(Scope& scope);

}  // namespace schemepp

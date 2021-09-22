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
    // expt
    void initializeBuiltinMathProcedure(Scope& scope);
    // File Library
    void initializeBuiltinFileProcedure(Scope& scope);
    // Load Library
    // Process-Context Library
    void initializeBuiltinProcessContextProcedure(Scope& scope);
    // Read Library
    void initializeBuiltinReadProcedure(Scope& scope);
    // Case-Lambda Library
    // Char Library
    void initializeBuiltinCharProcedure(Scope& scope);
    // Complex Library
    void initializeBuiltinComplexProcedure(Scope& scope);
    // CxR Library
    // Eval Library
    // Inexact Library
    // Lazy Library
    // Repl Library
    // Time Library
    void initializeBuiltinTimeProcedure(Scope& scope);
    // Write Library
    void initializeBuiltinWriteProcedure(Scope& scope);

}  // namespace schemepp

// SPDX-License-Identifier: MIT

#include "Interface/Error.hpp"
#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <filesystem>
#include <fstream>

namespace schemepp {

    namespace fs = std::filesystem;

#define PREFIX "<builtin procedure> Builtin.FileLibrary."

    class DeleteFile final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "DeleteFile";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() != 1)
                throwWrongOperandCountError(ctx, 1, operands.size());
            auto& path = asString(operands[0]);
            return constantBoolean(fs::exists(path) && fs::is_regular_file(path) && fs::remove_all(path));
        }
    };

    class FileExists final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "FileExists";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() != 1)
                throwWrongOperandCountError(ctx, 1, operands.size());
            auto& path = asString(operands[0]);
            return constantBoolean(fs::exists(path) && fs::is_regular_file(path));
        }
    };

    class InputFile final : public InputPort {
        std::string mPath;
        mutable std::ifstream mInput;

    public:
        InputFile(std::string path, const bool binary)
            : mPath{ std::move(path) }, mInput{ path, binary ? (std::ios::in | std::ios::binary) : std::ios::in } {
            if(!mInput.is_open())
                throw Error{ "Failed to open file " + mPath };
        }
        void printValue(std::ostream& stream) const override {
            stream << "InputFile " << mPath;
        }
        std::istream& input() const override {
            return mInput;
        }
    };

    class OutputFile final : public OutputPort {
        std::string mPath;
        mutable std::ofstream mOutput;

    public:
        OutputFile(std::string path, const bool binary)
            : mPath{ std::move(path) }, mOutput{ path, binary ? (std::ios::out | std::ios::binary) : std::ios::out } {
            if(!mOutput.is_open())
                throw Error{ "Failed to open file " + mPath };
        }
        void printValue(std::ostream& stream) const override {
            stream << "OutputFile " << mPath;
        }
        std::ostream& output() const override {
            return mOutput;
        }
    };

    template <bool Binary>
    class OpenInputFile final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "OpenInputFile";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() != 1)
                throwWrongOperandCountError(ctx, 1, operands.size());
            auto& path = asString(operands[0]);
            return makeRefCount<InputFile>(path, Binary);
        }
    };

    template <bool Binary>
    class OpenOutputFile final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "OpenOutputFile";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() != 1)
                throwWrongOperandCountError(ctx, 1, operands.size());
            auto& path = asString(operands[0]);
            return makeRefCount<OutputFile>(path, Binary);
        }
    };

    void initializeBuiltinFileProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)

        ADD_BUILTIN_PROCEDURE("delete-file", DeleteFile);
        ADD_BUILTIN_PROCEDURE("file-exists?", FileExists);
        ADD_BUILTIN_PROCEDURE("open-binary-input-file", OpenInputFile<true>);
        ADD_BUILTIN_PROCEDURE("open-binary-output-file", OpenOutputFile<true>);
        ADD_BUILTIN_PROCEDURE("open-input-file", OpenInputFile<false>);
        ADD_BUILTIN_PROCEDURE("open-output-file", OpenOutputFile<false>);

#undef ADD_BUILTIN_PROCEDURE
    }
}  // namespace schemepp

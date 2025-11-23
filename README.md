# ICD22 Homework3
# 【114 Autumn】555011編譯器 Compilers
## 513557008 鄭漢維 作業三
# 1. Makefile
- 改用 g++，因為程式使用了 C++14 語法與標準庫

        CC = g++ #gcc

# 2. Parser Overview
## 文法結構重點
- prog: 程式入口，包含 `declarations、subprogram_declarations、compound_statement`
- declarations: 變數宣告，會加入符號表
- subprogram_head: 函式/程序宣告，支援參數與返回型別
- mcompound_statement: `BEGIN ... END` 區塊
- parameter_list: 左遞迴支援多個參數，使用 `%prec LOWER_SEMICOLON` 避免 shift/reduce 衝突
- expression: 支援布林運算 `(AND / OR)`、比較運算與算術運算
- variable / tail: 支援陣列索引，並檢查型別一致性
##  型別與符號表檢查
- 支援 `INT / REAL / STRING / ARRAY / SUBPROG` 型別
- 檢查函式返回值、陣列索引、未宣告變數
- 自動收集函式參數型別並加入符號表
- 算術運算、邏輯運算與比較運算皆做型別兼容性檢查

# 3. Symbol Table Header (symbol_table.h)
## 功能概述

- 這個檔案提供了一個 C++ 符號表管理器，用於編譯器或語言處理器，主要功能包括：
    - 符號表管理：
        - 支援多層作用域 (scope)。
        - 可追蹤變數、常數、陣列、函式及其型別資訊。
        - 提供符號新增、查找、作用域開關等功能。
    - 型別描述 (TypeDescriptor)：
        - 支援基本型別 `INT、REAL、STRING、VOID`。
        - 支援陣列型別 `ARRAY`（含上下界與基底型別）。
        - 支援子程序型別 `SUBPROG`（函式型別與參數型別鏈結）。
    - 語意檢查：
        - 陣列索引檢查、型別相容性檢查。
        - 算術運算型別檢查。
        - 函式呼叫參數數量檢查。
        - 重複定義檢查與未宣告變數警告。
    - 符號表輸出：
        - 可以按時間戳或作用域Print符號表，用於Debug或分析。

## How port your homework2 to homework3

The scanner and parser is almost the same, you can refer the templates to modify your code.

In the assignment, you need to define some nodes to construct your Abstract Syntax Tree(AST) by your parser, then generate the symbol table and do some semantic analyses on it.

The format of messages that you need to print have already been defined in `include/info.h`, some warnings about semantic errors should be output to `stderr`, and the symbol table related messages need to be output to `stdout`, as stated in the header file.

Hints:

0. You may construct the AST first
1. Make symbol table by traversing your AST
2. Do semantic analysis, don't forget type check

Notes:

0. You can use the compare.sh to diff you output, use `bash compare.sh [testcase number]` or `bash compare.sh all`(wrong case only) to open `vimdiff` window, and you can type `ESC` and type `:qa!` to exit. If you want to stop the comparison, use `ctrl + z` to stop the process, and `kill %1` to kill it.
1. Assignments of function are only allowed in its compound statement, other function assignments would be treated as type error.


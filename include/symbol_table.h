#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <algorithm>
#include <cassert>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <stack>
#include <vector>
#include "ast.h" 
#include "info.h"


struct TypeDescriptor {
    IDType id_type;
    int lower_bound;
    int upper_bound;
    TypeDescriptor *base=nullptr;
    TypeDescriptor *next=nullptr;
};

struct SymbolTableEntry {
    int timestamp;
    TypeDescriptor *type_descriptor;
};

struct SymbolTableResult {
    int timestamp;
    TypeDescriptor *type_descriptor;
    int scope;
};


static std::string get_type_str(TypeDescriptor *type){
    if(!type) return "UNKNOWN";
    switch(type->id_type){
        case IDType::INT: return "int";
        case IDType::REAL: return "real";
        case IDType::STRING: return "string";
        case IDType::VOID: return "void";
        case IDType::ARRAY: {
            return get_type_str(type->base)+"["+
            std::to_string(type->lower_bound)+"~"+std::to_string(type->upper_bound)+"]";
            
        }
        case IDType::SUBPROG:{
            //std::string s= "(";
            std::string s;
            s += get_type_str(type->base);
            //if(type->base->next==nullptr) return s;
            if(!type->next) return s;
            s+=" (";
            for(TypeDescriptor *p=type->next;p!=nullptr;p=p->next){
                s+=get_type_str(p);
                if(p->next) s+=", ";
            }
            s+=")";
            return s;
        }
        default: return "UNKNOWN";
    }
}



struct SymbolTable {
    int curr_timestamp = 0;
    int curr_scope = -1;
    int max_scope = -1;
    bool semantic_error = false;
    bool is_arry_op=false;
    bool is_function=false;
    bool function_has_return=false;
    bool redefined_check=false;
    bool array_return_type=false;
    bool undeclared_variable=false;
    std::string current_function_name;
    std::string func_return_type;
    std::vector<std::unordered_map<std::string, SymbolTableEntry>> symbol_table;
    std::unordered_map<std::string,int> function_param_count;
    std::unordered_set<Node*> processed_node;
    std::unordered_set<std::string> seen;
    std::unordered_set<std::string> local_seen;
    std::unordered_map<std::string,int>local_seen_table;
    std::unordered_map<std::string,int>duplicated_func;
    std::unordered_map<std::string,std::pair<int,int>> undeclared_table;
    struct IdInfo {
    std::string name;
    uint32_t line;
    uint32_t col;
     TypeDescriptor* type;
    };
    std::vector<IdInfo> ids;

    SymbolTableResult add(const std::string &identifier, TypeDescriptor *type_descriptor) {
        auto it = symbol_table[curr_scope].find(identifier);
        if (symbol_table[curr_scope].count(identifier)) {
            if(type_descriptor->id_type==IDType::SUBPROG){
                duplicated_func[identifier]++;
            }
            return {it->second.timestamp,it->second.type_descriptor,curr_scope};
      
        } else {
            SHOW_NEWSYM(identifier.c_str());
            curr_timestamp++;
            //printf("%s,: %d\n",identifier.c_str(),curr_timestamp);
            symbol_table[curr_scope][identifier] = SymbolTableEntry{curr_timestamp, type_descriptor};
            return {curr_timestamp, type_descriptor, curr_scope};
        }
    }
    
    SymbolTableResult get(const std::string &identifier) {
        for (int i = curr_scope; i >= 0; i--) {
            if (symbol_table[i].count(identifier)) {
                SymbolTableEntry symbol_table_entry = symbol_table[i][identifier];
                return {symbol_table_entry.timestamp, symbol_table_entry.type_descriptor, i};
            }
        }
        assert(false);
    }

    void open_scope() {
        SHOW_NEWSCP();
        curr_scope++;
        max_scope=max_scope>=curr_scope?max_scope:curr_scope;
        if ((int)symbol_table.size() <= curr_scope)
            symbol_table.emplace_back();
        else
            symbol_table[curr_scope].clear();
    }

    void close_scope() {
        SHOW_CLSSCP();
        curr_scope--;
        //symbol_table.pop_back();
    }
    
    void sub_close_scope() {
        SHOW_CLSSCP();
        curr_scope--;
        print_symbol_table();
        symbol_table.pop_back();
    }

    void print_symbol_table() {
        SHOW_SYMTAB_HEAD();
        std::vector<std::tuple<std::string,SymbolTableEntry,int>> entries;
        for (int i = 0; i <= max_scope; ++i){
            //std::vector<std::pair<std::string,SymbolTableEntry>> entries;
            for(auto & entry:symbol_table[i]){
                entries.push_back(std::make_tuple(entry.first,entry.second,i));
            }
        }
            std::sort(entries.begin(),entries.end(),
                [](const auto &a,const auto &b){return std::get<1>(a).timestamp>std::get<1>(b).timestamp;});

            for(auto it =entries.begin();it!=entries.end();it++){
                TypeDescriptor* td = std::get<1>(*it).type_descriptor;
                std::string type_str = get_type_str(td);
                printf(SYMTAB_ENTRY_FMT,std::get<0>(*it).c_str(),std::get<2>(*it),type_str.c_str());
            }
        SHOW_SYMTAB_TAIL();
    }
    TypeDescriptor* make_type_descriptor(Node* type_node){
        if(!type_node) return nullptr;
        if(type_node->metadata.tval == IDType::ARRAY){
            TypeDescriptor* td = new TypeDescriptor;
            td->id_type=IDType::ARRAY;
            td->lower_bound=type_node->child[0]->metadata.ival;
            td->upper_bound=type_node->child[1]->metadata.ival;
            td->base=make_type_descriptor(type_node->child[2]);
            td->next=nullptr;
            return td;
        }
        else if(type_node->metadata.tval == IDType::INT){
            TypeDescriptor* td = new TypeDescriptor;
            td->id_type=IDType::INT;
            td->lower_bound=0;
            td->upper_bound=0;
            td->base=nullptr;
            td->next=nullptr;
            return td;
        }
         else if(type_node->metadata.tval == IDType::REAL){
            TypeDescriptor* td = new TypeDescriptor;
            td->id_type=IDType::REAL;
            td->lower_bound=0;
            td->upper_bound=0;
            td->base=nullptr;
            td->next=nullptr;
            return td;
        }
        else if(type_node->node_type == NodeType::LITERAL_DBL) {
            TypeDescriptor* td = new TypeDescriptor;
            td->id_type = IDType::REAL;
            td->base = nullptr;
            td->next = nullptr;
            td->lower_bound = td->upper_bound = 0;
            return td;
        }
        else if(type_node->node_type == NodeType::LITERAL_INT) {
            TypeDescriptor* td = new TypeDescriptor;
            td->id_type = IDType::INT;
            td->base = nullptr;
            td->next = nullptr;
            td->lower_bound = td->upper_bound = 0;
            return td;
        }
        else if(type_node->node_type == NodeType::SUBPROG_HEAD){
            TypeDescriptor* td = new TypeDescriptor;
            TypeDescriptor* return_type=nullptr;
            if(type_node->child[1])
                return_type=make_type_descriptor(type_node->child[1]);
            TypeDescriptor* param_types=collect_function_param_types(type_node->child[0]);
            td->id_type = IDType::SUBPROG;
            td->base = return_type? return_type:nullptr;
            td->next = param_types?param_types:nullptr;
            td->lower_bound = td->upper_bound = 0;
            return td;
        }
        else{
            TypeDescriptor* td=new TypeDescriptor;
            td->id_type =type_node->metadata.tval;
            td->lower_bound=0;
            td->upper_bound=0;
            td->base=nullptr;
            td->next=nullptr;
            return td;
        }
    }
    // helper function: 遞歸加入符號表
    void add_declarations(Node* decl) {
        if (!decl){ 
            return;
        }
        // 處理當前 identifier list
        Node* id = decl->child[0]; // 對應 $2
        Node* type_node = decl->child[1];
        TypeDescriptor* base_type = make_type_descriptor(type_node);

        Node* idlist = id;
        std::stack<IdInfo> var_stack;
            while (idlist) {           
                std::string name(idlist->metadata.sval);
                TypeDescriptor* prev = lookup_in_current_scope(name); 
                if(processed_node.find(idlist)==processed_node.end()){
                    processed_node.insert(idlist);
                    local_seen.insert(name);           
                }
            
                if (idlist->metadata.sval && idlist->metadata.sval[0] != '\0') {
                    var_stack.push({
                        std::string(idlist->metadata.sval),
                        idlist->loc.first_line,
                        idlist->loc.first_column,
                         base_type
                    });
                }
                idlist = idlist->next;
            }
        while(!var_stack.empty()){
            ids.push_back(var_stack.top());
            var_stack.pop();
        }
        if(decl->next)
            add_declarations(decl->next);
    }  
    // 按 source code 順序排序（line優先，再column）
    void flush(){
        std::sort(ids.begin(), ids.end(), [](const IdInfo& a, const IdInfo& b) {
            if (a.line != b.line) return a.line < b.line;
            return a.col < b.col;
        });
        for(std::vector<IdInfo>::iterator it=ids.begin();it!=ids.end();it++){
            if(symbol_table[curr_scope].count((*it).name) && curr_scope>0 && local_seen_table[(*it).name]>0){
                fprintf(stderr, REDEF_VAR,
                        (*it).line, (*it).col, (*it).name.c_str());
                undeclared_variable=true;
            }
            bool duplicated = false;
            if(seen.count(it->name)) continue;
            for(std::vector<IdInfo>::iterator jt=ids.begin();jt!=it;jt++){            
                if((*it).name==(*jt).name){
                    fprintf(stderr,REDEF_VAR,
                        (*it).line, (*it).col, (*it).name.c_str());
                    duplicated=true;
                    break;
                }    
            }
            if(duplicated) seen.insert((*it).name);
        }
        for(auto &ii : ids){
            add(ii.name,ii.type);
        }
        ids.clear();
        local_seen.clear(); 
        local_seen_table.clear();
    }
    void add_function_params(Node* args,std::vector<std::string> &param_types) {
        if(!args) return;
        Node* param_list = args;

        for(Node *group=param_list;group!=nullptr;group=group->next){
            Node* idlist=group->child[0];
            Node* type_node=group->child[1];
            if(!type_node) continue;
            
            for(Node* id=idlist;id!=nullptr;id=id->next){
                TypeDescriptor *param_type = make_type_descriptor(type_node);
                std::string name(id->metadata.sval);
                add(name,param_type);
                std::string name_check(name);
                local_seen_table[name]++;
                param_types.push_back(get_type_str(param_type));
            }
        }


    }

    TypeDescriptor* collect_function_param_types(Node* params) {
        if (!params) return nullptr;

        TypeDescriptor *head = nullptr;
        TypeDescriptor *tail = nullptr;
        // params 是 parameter_list
        Node* p = params;
        while (p) {
            Node* idlist = p->child[0];   // 變數名稱列表
            Node* type_node = p->child[1]; // 型別節點
            for(Node*id=idlist;id!=nullptr;id=id->next){
                TypeDescriptor* param_type = make_type_descriptor(type_node);
                // 依序鏈結參數型別
                if (!head) head = param_type;
                else tail->next = param_type;
                tail = param_type;
            }
            p = p->next; // parameter_list 可能以 next 串起來
        }
        return head;
    }

    TypeDescriptor* lookup(const std::string & name){
        for(int s = curr_scope;s>=0;--s){
            auto it =symbol_table[s].find(name);
            if(it!=symbol_table[s].end()){
                //return cloneType(it->second.type_descriptor);
                return it->second.type_descriptor;
            }
        }
        return nullptr;
    }

    TypeDescriptor* cloneType(const TypeDescriptor *src){
        if(!src) return nullptr;
        TypeDescriptor* copy = new TypeDescriptor;
        copy->id_type=src->id_type;
        copy->lower_bound=src->lower_bound;
        copy->upper_bound=src->upper_bound;
        copy->base=cloneType(src->base);
        copy->next=cloneType(src->next);
        return copy;
    }
    TypeDescriptor* lookup_in_current_scope(const std::string & name){
        auto it =symbol_table[curr_scope].find(name);
        if(it!=symbol_table[curr_scope].end()) return cloneType (it->second.type_descriptor);
        return nullptr;
    }
    void check_array(Node* var_node){
        TypeDescriptor* type = lookup(var_node->metadata.sval);
        Node* sub_list = var_node->child[0];
        int sub_count=0;
        TypeDescriptor* t =type;

        for(Node*e = sub_list;e!=nullptr;e=e->next){
            sub_count++;
 
            if(!t||t->id_type!=IDType::ARRAY){
                fprintf(stderr, INDEX_MANY,
                        var_node->loc.first_line,
                        var_node->loc.first_column,
                        var_node->metadata.sval);
                return;
            }       
            TypeDescriptor* idx_type=nullptr;      
            t= t->base;
        }
        if(t&& t->id_type==IDType::ARRAY){      
            fprintf(stderr, ASSIG_TYPE,
                var_node->loc.first_line,
                var_node->loc.first_column);
        }
    }

    TypeDescriptor* get_exp_str(Node *node){
        if(!node) return nullptr;
        switch(node->node_type){
            case NodeType::LITERAL_INT:
                return new TypeDescriptor{IDType::INT,0,0,nullptr,nullptr};
            case NodeType::LITERAL_DBL:
                return new TypeDescriptor{IDType::REAL,0,0,nullptr,nullptr};
            case NodeType::VAR:{
                TypeDescriptor* res = lookup(node->metadata.sval);
                TypeDescriptor* td =res;
                if(td && td->id_type==IDType::ARRAY){
                    return new TypeDescriptor{IDType::ARRAY,res->lower_bound,res->upper_bound,res->base,nullptr};
                }
                if(td!=nullptr && td->id_type==IDType::SUBPROG){
                    if(!(td->base)) return nullptr;
                    return td->base;
                }else{
                    return td;
                }
            }
            case NodeType::NEGATE:{
                auto t =get_exp_str(node->child[0]);
                if(is_arry_op==true)
                    return new TypeDescriptor{IDType::ARRAY,0,0,nullptr,nullptr};
                if(!t) return nullptr;
                if(t->id_type==IDType::INT || t->id_type==IDType::REAL)
                    return t;
            }
            case NodeType::OP:{
                if(node->metadata.tval==IDType::ARRAY || is_arry_op==true)
                {
                    return new TypeDescriptor{IDType::ARRAY,0,0,nullptr,nullptr};
                }
                auto lhs = get_exp_str(node->child[0]);
                auto rhs = get_exp_str(node->child[1]);

                if(!lhs || !rhs) return nullptr;
                if(lhs->id_type==IDType::ARRAY && rhs->id_type==IDType::ARRAY){
                    return new TypeDescriptor{IDType::ARRAY,0,0,nullptr,nullptr};
                }
                if(lhs->id_type==IDType::INT && rhs->id_type==IDType::INT){
                    return new TypeDescriptor{IDType::INT,0,0,nullptr,nullptr};
                }
                if(lhs->id_type==IDType::REAL && rhs->id_type==IDType::REAL){
                    return new TypeDescriptor{IDType::REAL,0,0,nullptr,nullptr};
                }
                if(lhs->id_type==IDType::ARRAY){
                    TypeDescriptor * lhs_base=get_base_type(lhs);
                    if(!lhs->next) return nullptr;
                    if(lhs_base->id_type==IDType::INT && rhs->id_type==IDType::INT){
                        return new TypeDescriptor{IDType::INT,0,0,nullptr,nullptr};
                    }
                    else if(lhs_base->id_type==IDType::REAL && rhs->id_type==IDType::REAL){
                        return new TypeDescriptor{IDType::INT,0,0,nullptr,nullptr};
                    }
                }
                if(rhs->id_type==IDType::ARRAY){
                    TypeDescriptor * rhs_base=get_base_type(rhs);
                    if(!rhs->next) return nullptr;
                    if(rhs_base->id_type==IDType::INT && lhs->id_type==IDType::INT){
                        return new TypeDescriptor{IDType::INT,0,0,nullptr,nullptr};
                    }
                    else if(rhs_base->id_type==IDType::REAL && lhs->id_type==IDType::REAL){
                        return new TypeDescriptor{IDType::REAL,0,0,nullptr,nullptr};
                    }
                }
                return nullptr;

            }
            default:return nullptr;
            }       
    }
    
    void check_procedure_type(Node* node,Node* node3){
        TypeDescriptor* type=lookup(node->metadata.sval);
        if(!type || type->id_type!=IDType::SUBPROG){
            fprintf(stderr, UNDEC_FUN,
                    node->loc.first_line,node->loc.first_column,node->metadata.sval);
        }
        else{
            TypeDescriptor* ret=type->base;
            //TypeDescriptor* formal=ret?ret->next:nullptr;
            TypeDescriptor* formal=type->next;
            int formal_count=0;
            for(TypeDescriptor* f=formal;f;f=f->next){
                formal_count++;
            } 
            int actual_count=0;
            for(Node*p=node3;p!=nullptr;p=p->next)
                actual_count++;
            if(formal_count!=actual_count){
                fprintf(stderr, WRONG_ARGS,
                          node->loc.first_line,node->loc.first_column,node->metadata.sval);
            }
        }
    }
    void check_array_index(Node* exp_list){
        for(Node *p=exp_list;p!=nullptr;p=p->next){
            if(!p->child[0]) continue;       
            Node*var_p=p->child[0]; 
            TypeDescriptor*idx = nullptr;
            Node* var_node = find_var(var_p);
            if(var_node){
                idx=lookup(var_node->metadata.sval);
            }
            else{
                idx=make_type_descriptor(var_p);
            }
            if(!idx) continue;
            if(idx->id_type==IDType::REAL){
                 fprintf(stderr, INDEX_TYPE, p->loc.first_line, p->loc.first_column);
            }
        }
    }

    Node *find_var(Node* var){
        if(!var) return nullptr;
        if(var->node_type==NodeType::VAR)
        {   
            return var;
        }
        for(int i=0;i<4;i++){
                Node *res=find_var(var->child[i]);
                if(res) return res;  
        }
        return nullptr;
    }

     Node *find_func(Node* var){
        if(!var) return nullptr;
        if(var->node_type==NodeType::SUBPROG_HEAD)
        {   
           
            return var;
        }
        for(int i=0;i<4;i++){

                Node *res=find_var(var->child[i]);
                if(res) return res;
    
        }
        return nullptr;
    }

    int actual_args(Node* exp_list){
        int c=0;
        Node*p=exp_list;
        while(p!=nullptr){
            c++;
            p=p->next;
        }
        return c;
    }
    TypeDescriptor* get_base_type(TypeDescriptor*t){
        //while(t&&t->base)
        while(t->base && t->base->id_type==IDType::ARRAY)
            t=t->base;
        return t;
    }
    const char* id_type_to_str(IDType t){
        switch(t){
            case IDType::INT: return "int";
            case IDType::REAL: return "real";
            case IDType::VOID: return "void";
            case IDType::SUBPROG: return "subprogram";
            case IDType::ARRAY: return "array";
            default: return "unknown";
        }
    }
    void check_type_compatibility(TypeDescriptor* lhs_type, TypeDescriptor* rhs_type, const LocType & loc){
        if((lhs_type->id_type==IDType::INT && rhs_type->id_type==IDType::REAL) ||
            (lhs_type->id_type==IDType::REAL && rhs_type->id_type==IDType::INT)){
                fprintf(stderr,ASSIG_TYPE,(loc).first_line,(loc).first_column);
            }
    }
    void check_array_type_compatibility(TypeDescriptor* lhs_type, TypeDescriptor* rhs_type, const LocType & loc){
        if(!(lhs_type->id_type==IDType::ARRAY && lhs_type->base)){
            return;
        }
        else{
            if(lhs_type->base->id_type==IDType::INT && 
                !((rhs_type->id_type==IDType::ARRAY && rhs_type->base && 
                    rhs_type->base->id_type==IDType::INT) ||
                    (rhs_type->id_type==IDType::INT))){
                        fprintf(stderr,ASSIG_TYPE,(loc).first_line,(loc).first_column);
            }
            else if(lhs_type->base->id_type==IDType::REAL && 
                !((rhs_type->id_type==IDType::ARRAY && rhs_type->base && 
                    rhs_type->base->id_type==IDType::REAL) ||
                    (rhs_type->id_type==IDType::REAL))){
                        fprintf(stderr,ASSIG_TYPE,(loc).first_line,(loc).first_column);
            }
        }
    }
    void check_arithmetic_type(TypeDescriptor* lhs_type, TypeDescriptor* rhs_type, OpType op, const LocType & loc){
        bool mismatch = 
            (lhs_type->id_type == IDType::INT && rhs_type->id_type==IDType::REAL) ||
            (lhs_type->id_type == IDType::REAL && rhs_type->id_type==IDType::INT);
        if(mismatch){
            const char *op_str = (op == OpType::ADD ? "+" :
                                (op == OpType::SUB ? "-" :
                                (op == OpType::MUL ? "*" :
                                (op == OpType::DIV ? "/" : "?"))));
            fprintf(stderr, ARITH_TYPE, loc.first_line, loc.first_column,op_str);
        }
    }
     void check_non_arithmetic_type(TypeDescriptor* lhs_type, TypeDescriptor* rhs_type, OpType op, const LocType & loc){
        bool mismatch = 
            !((lhs_type->id_type == IDType::INT || lhs_type->id_type==IDType::REAL) &&
            (rhs_type->id_type == IDType::INT || rhs_type->id_type==IDType::REAL));
        if(mismatch){
            const char *op_str = (op == OpType::ADD ? "+" :
                                (op == OpType::SUB ? "-" :
                                (op == OpType::MUL ? "*" :
                                (op == OpType::DIV ? "/" : "?"))));
            fprintf(stderr, ARITH_TYPE, loc.first_line, loc.first_column,op_str);
        }
    }
    

};

#endif

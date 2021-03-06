// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_lazyarray_RCPPEXPORTS_H_GEN_
#define RCPP_lazyarray_RCPPEXPORTS_H_GEN_

#include <Rcpp.h>

namespace lazyarray {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("lazyarray", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("lazyarray", "_lazyarray_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in lazyarray");
            }
        }
    }

    inline R_xlen_t setLazyBlockSize(R_xlen_t size) {
        typedef SEXP(*Ptr_setLazyBlockSize)(SEXP);
        static Ptr_setLazyBlockSize p_setLazyBlockSize = NULL;
        if (p_setLazyBlockSize == NULL) {
            validateSignature("R_xlen_t(*setLazyBlockSize)(R_xlen_t)");
            p_setLazyBlockSize = (Ptr_setLazyBlockSize)R_GetCCallable("lazyarray", "_lazyarray_setLazyBlockSize");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_setLazyBlockSize(Shield<SEXP>(Rcpp::wrap(size)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<R_xlen_t >(rcpp_result_gen);
    }

    inline R_xlen_t getLazyBlockSize() {
        typedef SEXP(*Ptr_getLazyBlockSize)();
        static Ptr_getLazyBlockSize p_getLazyBlockSize = NULL;
        if (p_getLazyBlockSize == NULL) {
            validateSignature("R_xlen_t(*getLazyBlockSize)()");
            p_getLazyBlockSize = (Ptr_getLazyBlockSize)R_GetCCallable("lazyarray", "_lazyarray_getLazyBlockSize");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_getLazyBlockSize();
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<R_xlen_t >(rcpp_result_gen);
    }

    inline std::vector<int64_t> loc2idx3(SEXP locations, std::vector<int64_t>& parent_dim) {
        typedef SEXP(*Ptr_loc2idx3)(SEXP,SEXP);
        static Ptr_loc2idx3 p_loc2idx3 = NULL;
        if (p_loc2idx3 == NULL) {
            validateSignature("std::vector<int64_t>(*loc2idx3)(SEXP,std::vector<int64_t>&)");
            p_loc2idx3 = (Ptr_loc2idx3)R_GetCCallable("lazyarray", "_lazyarray_loc2idx3");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_loc2idx3(Shield<SEXP>(Rcpp::wrap(locations)), Shield<SEXP>(Rcpp::wrap(parent_dim)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<std::vector<int64_t> >(rcpp_result_gen);
    }

    inline Rcpp::List extractSlices(SEXP listOrEnv, const R_xlen_t& ndims) {
        typedef SEXP(*Ptr_extractSlices)(SEXP,SEXP);
        static Ptr_extractSlices p_extractSlices = NULL;
        if (p_extractSlices == NULL) {
            validateSignature("Rcpp::List(*extractSlices)(SEXP,const R_xlen_t&)");
            p_extractSlices = (Ptr_extractSlices)R_GetCCallable("lazyarray", "_lazyarray_extractSlices");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_extractSlices(Shield<SEXP>(Rcpp::wrap(listOrEnv)), Shield<SEXP>(Rcpp::wrap(ndims)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::List >(rcpp_result_gen);
    }

    inline Rcpp::List parseSlices(SEXP listOrEnv, const std::vector<int64_t>& dim, bool pos_subscript = true) {
        typedef SEXP(*Ptr_parseSlices)(SEXP,SEXP,SEXP);
        static Ptr_parseSlices p_parseSlices = NULL;
        if (p_parseSlices == NULL) {
            validateSignature("Rcpp::List(*parseSlices)(SEXP,const std::vector<int64_t>&,bool)");
            p_parseSlices = (Ptr_parseSlices)R_GetCCallable("lazyarray", "_lazyarray_parseSlices");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_parseSlices(Shield<SEXP>(Rcpp::wrap(listOrEnv)), Shield<SEXP>(Rcpp::wrap(dim)), Shield<SEXP>(Rcpp::wrap(pos_subscript)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::List >(rcpp_result_gen);
    }

    inline Rcpp::List parseAndScheduleBlocks2(SEXP sliceIdx, Rcpp::NumericVector dim, bool forceSchedule = false) {
        typedef SEXP(*Ptr_parseAndScheduleBlocks2)(SEXP,SEXP,SEXP);
        static Ptr_parseAndScheduleBlocks2 p_parseAndScheduleBlocks2 = NULL;
        if (p_parseAndScheduleBlocks2 == NULL) {
            validateSignature("Rcpp::List(*parseAndScheduleBlocks2)(SEXP,Rcpp::NumericVector,bool)");
            p_parseAndScheduleBlocks2 = (Ptr_parseAndScheduleBlocks2)R_GetCCallable("lazyarray", "_lazyarray_parseAndScheduleBlocks2");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_parseAndScheduleBlocks2(Shield<SEXP>(Rcpp::wrap(sliceIdx)), Shield<SEXP>(Rcpp::wrap(dim)), Shield<SEXP>(Rcpp::wrap(forceSchedule)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::List >(rcpp_result_gen);
    }

    inline SEXP reshapeOrDrop(SEXP x, SEXP reshape = R_NilValue, bool drop = false) {
        typedef SEXP(*Ptr_reshapeOrDrop)(SEXP,SEXP,SEXP);
        static Ptr_reshapeOrDrop p_reshapeOrDrop = NULL;
        if (p_reshapeOrDrop == NULL) {
            validateSignature("SEXP(*reshapeOrDrop)(SEXP,SEXP,bool)");
            p_reshapeOrDrop = (Ptr_reshapeOrDrop)R_GetCCallable("lazyarray", "_lazyarray_reshapeOrDrop");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_reshapeOrDrop(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(reshape)), Shield<SEXP>(Rcpp::wrap(drop)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP subsetFST(const std::string& rootPath, SEXP listOrEnv, const std::vector<int64_t>& dim, SEXPTYPE dtype, SEXP reshape = R_NilValue, bool drop = false) {
        typedef SEXP(*Ptr_subsetFST)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_subsetFST p_subsetFST = NULL;
        if (p_subsetFST == NULL) {
            validateSignature("SEXP(*subsetFST)(const std::string&,SEXP,const std::vector<int64_t>&,SEXPTYPE,SEXP,bool)");
            p_subsetFST = (Ptr_subsetFST)R_GetCCallable("lazyarray", "_lazyarray_subsetFST");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_subsetFST(Shield<SEXP>(Rcpp::wrap(rootPath)), Shield<SEXP>(Rcpp::wrap(listOrEnv)), Shield<SEXP>(Rcpp::wrap(dim)), Shield<SEXP>(Rcpp::wrap(dtype)), Shield<SEXP>(Rcpp::wrap(reshape)), Shield<SEXP>(Rcpp::wrap(drop)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP scheduleFST(SEXP listOrEnv, const std::vector<int64_t>& dim, bool forceSchedule = false, int64_t hint = -1) {
        typedef SEXP(*Ptr_scheduleFST)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_scheduleFST p_scheduleFST = NULL;
        if (p_scheduleFST == NULL) {
            validateSignature("SEXP(*scheduleFST)(SEXP,const std::vector<int64_t>&,bool,int64_t)");
            p_scheduleFST = (Ptr_scheduleFST)R_GetCCallable("lazyarray", "_lazyarray_scheduleFST");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_scheduleFST(Shield<SEXP>(Rcpp::wrap(listOrEnv)), Shield<SEXP>(Rcpp::wrap(dim)), Shield<SEXP>(Rcpp::wrap(forceSchedule)), Shield<SEXP>(Rcpp::wrap(hint)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP executeScheduleFST(const std::string& rootPath, SEXPTYPE dtype, SEXP reshape, bool drop, int64_t partition) {
        typedef SEXP(*Ptr_executeScheduleFST)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_executeScheduleFST p_executeScheduleFST = NULL;
        if (p_executeScheduleFST == NULL) {
            validateSignature("SEXP(*executeScheduleFST)(const std::string&,SEXPTYPE,SEXP,bool,int64_t)");
            p_executeScheduleFST = (Ptr_executeScheduleFST)R_GetCCallable("lazyarray", "_lazyarray_executeScheduleFST");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_executeScheduleFST(Shield<SEXP>(Rcpp::wrap(rootPath)), Shield<SEXP>(Rcpp::wrap(dtype)), Shield<SEXP>(Rcpp::wrap(reshape)), Shield<SEXP>(Rcpp::wrap(drop)), Shield<SEXP>(Rcpp::wrap(partition)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP scheduleExistsFST() {
        typedef SEXP(*Ptr_scheduleExistsFST)();
        static Ptr_scheduleExistsFST p_scheduleExistsFST = NULL;
        if (p_scheduleExistsFST == NULL) {
            validateSignature("SEXP(*scheduleExistsFST)()");
            p_scheduleExistsFST = (Ptr_scheduleExistsFST)R_GetCCallable("lazyarray", "_lazyarray_scheduleExistsFST");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_scheduleExistsFST();
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP freeScheduleFST() {
        typedef SEXP(*Ptr_freeScheduleFST)();
        static Ptr_freeScheduleFST p_freeScheduleFST = NULL;
        if (p_freeScheduleFST == NULL) {
            validateSignature("SEXP(*freeScheduleFST)()");
            p_freeScheduleFST = (Ptr_freeScheduleFST)R_GetCCallable("lazyarray", "_lazyarray_freeScheduleFST");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_freeScheduleFST();
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline int getLazyThread(bool max = false) {
        typedef SEXP(*Ptr_getLazyThread)(SEXP);
        static Ptr_getLazyThread p_getLazyThread = NULL;
        if (p_getLazyThread == NULL) {
            validateSignature("int(*getLazyThread)(bool)");
            p_getLazyThread = (Ptr_getLazyThread)R_GetCCallable("lazyarray", "_lazyarray_getLazyThread");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_getLazyThread(Shield<SEXP>(Rcpp::wrap(max)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<int >(rcpp_result_gen);
    }

    inline int setLazyThread(int n, SEXP reset_after_fork = R_NilValue) {
        typedef SEXP(*Ptr_setLazyThread)(SEXP,SEXP);
        static Ptr_setLazyThread p_setLazyThread = NULL;
        if (p_setLazyThread == NULL) {
            validateSignature("int(*setLazyThread)(int,SEXP)");
            p_setLazyThread = (Ptr_setLazyThread)R_GetCCallable("lazyarray", "_lazyarray_setLazyThread");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_setLazyThread(Shield<SEXP>(Rcpp::wrap(n)), Shield<SEXP>(Rcpp::wrap(reset_after_fork)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<int >(rcpp_result_gen);
    }

    inline bool hasOpenMP() {
        typedef SEXP(*Ptr_hasOpenMP)();
        static Ptr_hasOpenMP p_hasOpenMP = NULL;
        if (p_hasOpenMP == NULL) {
            validateSignature("bool(*hasOpenMP)()");
            p_hasOpenMP = (Ptr_hasOpenMP)R_GetCCallable("lazyarray", "_lazyarray_hasOpenMP");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_hasOpenMP();
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline SEXP subsetAssignFST(const SEXP values, const std::string& file, SEXP listOrEnv, const std::vector<int64_t>& dim, const SEXPTYPE& dtype, int compression = 50, bool uniformEncoding = true) {
        typedef SEXP(*Ptr_subsetAssignFST)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_subsetAssignFST p_subsetAssignFST = NULL;
        if (p_subsetAssignFST == NULL) {
            validateSignature("SEXP(*subsetAssignFST)(const SEXP,const std::string&,SEXP,const std::vector<int64_t>&,const SEXPTYPE&,int,bool)");
            p_subsetAssignFST = (Ptr_subsetAssignFST)R_GetCCallable("lazyarray", "_lazyarray_subsetAssignFST");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_subsetAssignFST(Shield<SEXP>(Rcpp::wrap(values)), Shield<SEXP>(Rcpp::wrap(file)), Shield<SEXP>(Rcpp::wrap(listOrEnv)), Shield<SEXP>(Rcpp::wrap(dim)), Shield<SEXP>(Rcpp::wrap(dtype)), Shield<SEXP>(Rcpp::wrap(compression)), Shield<SEXP>(Rcpp::wrap(uniformEncoding)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP dropDimension(SEXP x) {
        typedef SEXP(*Ptr_dropDimension)(SEXP);
        static Ptr_dropDimension p_dropDimension = NULL;
        if (p_dropDimension == NULL) {
            validateSignature("SEXP(*dropDimension)(SEXP)");
            p_dropDimension = (Ptr_dropDimension)R_GetCCallable("lazyarray", "_lazyarray_dropDimension");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_dropDimension(Shield<SEXP>(Rcpp::wrap(x)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline int64_t prod2(SEXP x, bool na_rm = false) {
        typedef SEXP(*Ptr_prod2)(SEXP,SEXP);
        static Ptr_prod2 p_prod2 = NULL;
        if (p_prod2 == NULL) {
            validateSignature("int64_t(*prod2)(SEXP,bool)");
            p_prod2 = (Ptr_prod2)R_GetCCallable("lazyarray", "_lazyarray_prod2");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_prod2(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(na_rm)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<int64_t >(rcpp_result_gen);
    }

    inline SEXP parseDots(Rcpp::Environment& env, bool eval) {
        typedef SEXP(*Ptr_parseDots)(SEXP,SEXP);
        static Ptr_parseDots p_parseDots = NULL;
        if (p_parseDots == NULL) {
            validateSignature("SEXP(*parseDots)(Rcpp::Environment&,bool)");
            p_parseDots = (Ptr_parseDots)R_GetCCallable("lazyarray", "_lazyarray_parseDots");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_parseDots(Shield<SEXP>(Rcpp::wrap(env)), Shield<SEXP>(Rcpp::wrap(eval)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline bool stopIfNot(const bool isValid, const std::string& message, bool stopIfError = true) {
        typedef SEXP(*Ptr_stopIfNot)(SEXP,SEXP,SEXP);
        static Ptr_stopIfNot p_stopIfNot = NULL;
        if (p_stopIfNot == NULL) {
            validateSignature("bool(*stopIfNot)(const bool,const std::string&,bool)");
            p_stopIfNot = (Ptr_stopIfNot)R_GetCCallable("lazyarray", "_lazyarray_stopIfNot");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_stopIfNot(Shield<SEXP>(Rcpp::wrap(isValid)), Shield<SEXP>(Rcpp::wrap(message)), Shield<SEXP>(Rcpp::wrap(stopIfError)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline SEXPTYPE getSexpType(SEXP x) {
        typedef SEXP(*Ptr_getSexpType)(SEXP);
        static Ptr_getSexpType p_getSexpType = NULL;
        if (p_getSexpType == NULL) {
            validateSignature("SEXPTYPE(*getSexpType)(SEXP)");
            p_getSexpType = (Ptr_getSexpType)R_GetCCallable("lazyarray", "_lazyarray_getSexpType");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_getSexpType(Shield<SEXP>(Rcpp::wrap(x)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXPTYPE >(rcpp_result_gen);
    }

    inline SEXP tik() {
        typedef SEXP(*Ptr_tik)();
        static Ptr_tik p_tik = NULL;
        if (p_tik == NULL) {
            validateSignature("SEXP(*tik)()");
            p_tik = (Ptr_tik)R_GetCCallable("lazyarray", "_lazyarray_tik");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_tik();
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP tok(std::string msg, bool stop = false) {
        typedef SEXP(*Ptr_tok)(SEXP,SEXP);
        static Ptr_tok p_tok = NULL;
        if (p_tok == NULL) {
            validateSignature("SEXP(*tok)(std::string,bool)");
            p_tok = (Ptr_tok)R_GetCCallable("lazyarray", "_lazyarray_tok");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_tok(Shield<SEXP>(Rcpp::wrap(msg)), Shield<SEXP>(Rcpp::wrap(stop)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP subsetAssignVector(SEXP x, int64_t start, SEXP value) {
        typedef SEXP(*Ptr_subsetAssignVector)(SEXP,SEXP,SEXP);
        static Ptr_subsetAssignVector p_subsetAssignVector = NULL;
        if (p_subsetAssignVector == NULL) {
            validateSignature("SEXP(*subsetAssignVector)(SEXP,int64_t,SEXP)");
            p_subsetAssignVector = (Ptr_subsetAssignVector)R_GetCCallable("lazyarray", "_lazyarray_subsetAssignVector");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_subsetAssignVector(Shield<SEXP>(Rcpp::wrap(x)), Shield<SEXP>(Rcpp::wrap(start)), Shield<SEXP>(Rcpp::wrap(value)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

}

#endif // RCPP_lazyarray_RCPPEXPORTS_H_GEN_

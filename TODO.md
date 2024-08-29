# TODO

## std::mutex

   Add `std::mutex` under the flag `HB_USE_CPP_MUTEX`.
   Harbour++ v2: remove old code and use only std::mutex.
   Work in progress (not ready to use).

## src/vm/dynsym.cpp

   Use `std::vector` or other C++ container (under the flag `HB_USE_CPP_VECTOR`).

## src/rdd/dbcmd.cpp

  Changes planned for Harbour++ v2, but can be made in v1 (not decided yet):  
  Deprecate ALIAS, change ALIAS to DBALIAS and maintain ALIAS using HB_FUNC_TRANSLATE.  
  Deprecate BOF, change BOF to DBBOF and maintain BOF using HB_FUNC_TRANSLATE.  
  Deprecate DELETED, change DELETED to DBDELETED and maintain DELETED using HB_FUNC_TRANSLATE.  
  Deprecate EOF, change EOF to DBEOF and maintain EOF using HB_FUNC_TRANSLATE.  
  Deprecate FCOUNT, change FCOUNT to DBFCOUNT and maintain FCOUNT using HB_FUNC_TRANSLATE.  
  Deprecate FIELDGET, change FIELDGET to DBFIELDGET and maintain FIELDGET using HB_FUNC_TRANSLATE.  
  Deprecate FIELDNAME, change FIELDNAME to DBFIELDNAME and maintain FIELDNAME using HB_FUNC_TRANSLATE.  
  Deprecate FIELDPOS, change FIELDPOS to DBFIELDPOS and maintain FIELDPOS using HB_FUNC_TRANSLATE.  
  Deprecate FIELDPUT, change FIELDPUT to DBFIELDPUT and maintain FIELDPUT using HB_FUNC_TRANSLATE.  
  Deprecate FLOCK, change FLOCK to DBFLOCK and maintain FLOCK using HB_FUNC_TRANSLATE.  
  Deprecate FOUND, change FOUND to DBFOUND and maintain FOUND using HB_FUNC_TRANSLATOR.  
  Deprecate HEADER, change HEADER to DBHEADER and maintain HEADER using HB_FUNC_TRANSLATE.  
  Deprecate INDEXORD, change INDEXORD to DBINDEXORD and maintain INDEXORD using HB_FUNC_TRANSLATE.  
  Deprecate LASTREC, change LASTREC to DBLASTREC and maintain LASTREC using HB_FUNC_TRANSLATE.  
  Deprecate LUPDATE, change LUPDATE to DBLUPDATE and maintain LUPDATE using HB_FUNC_TRANSLATE.  
  Deprecate NETERR, change NETERR to DBNETERR and maintain NETERR using HB_FUNC_TRANSLATE.  
  Deprecate RECNO, change RECNO to DBRECNO and maintain RECNO using HB_FUNC_TRANSLATE.  
  Deprecate RECSIZE, change RECSIZE to DBRECSIZE and maintain RECSIZE using HB_FUNC_TRANSLATE.  
  Deprecate RLOCK, join with DBRLOCK and maintain RLOCK using HB_FUNC_TRANSLATE.  
  Deprecate SELECT, change SELECT to DBSELECT and maintain SELECT using HB_FUNC_TRANSLATE.  
  Deprecate USED, change USED to DBUSED and maintain USED using HB_FUNC_TRANSLATE.  

## src/vm/arrayshb.cpp

  Changes planned for Harbour++ v2, but can be made in v1 (not decided yet):  
  Deprecate HB_ASCAN, join ASCAN and HB_ASCAN and maintain HB_ASCAN using HB_FUNC_TRANSLATE.  
  Deprecate HB_AINS, join AINS and HB_AINS and maintain HB_AINS using HB_FUNC_TRANSLATE.  
  Deprecate HB_ADEL, join ADEL and HB_ADEL and maintain HB_ADEL using HB_FUNC_TRANSLATE.  

## src/rtl/at.cpp

  Changes planned for Harbour++ v2, but can be made in v1 (not decided yet):  
  Deprecate HB_AT, join AT and HB_AT and maintain HB_AT using HB_FUNC_TRANSLATE.  

## contrib

  Changes planned for Harbour++ v2, but can be made in v1 (not decided yet):  
  Move `contrib` folder to a separate repository.  

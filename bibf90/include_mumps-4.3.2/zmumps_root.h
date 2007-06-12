!
!   THIS FILE IS PART OF ZMUMPS VERSION 4.3.2
!   This Version was built on Wed Nov 12 16:57:09 2003
!
! COPYRIGHT (c) 1996-2003 P. R. Amestoy, I. S. Duff, J. Koster,
!                       J.-Y. L'Excellent
!
!  CERFACS      , Toulouse    (France)  (http://www.cerfacs.fr)
!  ENSEEIHT-IRIT, Toulouse    (France)  (http://www.enseeiht.fr)
!  INRIA                      (France)  (http://www.inria.fr)
!  PARALLAB     , Bergen      (Norway)  (http://www.parallab.uib.no)
!
! All rights reserved.
!
!  Your use or distribution of the package implies that you agree
!  with this License. Up-to-date copies of the ZMUMPS package can be
!  obtained from the Web page http://www.enseeiht.fr/apo/ZMUMPS/
!
!  This package is provided to you free of charge. It was
!  initially based on public domain software developed during
!  the European Esprit IV project PARASOL (1996-1999).
!
!  THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY
!  EXPRESSED OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
!
!  Permission is hereby granted to use or copy this
!  package provided that the Copyright and this License is
!  retained on all copies and that the package is used
!  under the same terms and conditions. User documentation
!  of any code that uses this software should include this
!  complete Copyright notice and this License.
!
!  You can modify this code but, at no time shall the right
!  or title to all or any part of this package pass to you.
!  All information relating to any alteration or addition
!  made to this package for the purposes of extending the
!  capabilities or enhancing the performance of this package
!  shall be made available free of charge to the authors for
!  any purpose.
!
!  You shall acknowledge (using references [1] and [2])
!  the contribution of this package in any publication
!  of material dependent upon the use of the package.
!  You shall use reasonable endeavours to notify
!  the authors of the package of this publication.
!
!
!  [1] P. R. Amestoy, I. S. Duff and  J.-Y. L'Excellent (1998),
!  Multifrontal parallel distributed symmetric and unsymmetric solvers,
!  in Comput. Methods in Appl. Mech. Eng., 184,  501-520 (2000).
!  An early version appeared as a Technical Report ENSEEIHT-IRIT (1998)
!  and is available at http://www.enseeiht.fr/apo/ZMUMPS/.
!
!  [2] P. R. Amestoy, I. S. Duff, J. Koster and  J.-Y. L'Excellent,
!  A fully asynchronous multifrontal solver using distributed dynamic
!  scheduling, SIAM Journal of Matrix Analysis and Applications,
!  Vol 23, No 1, pp 15-41 (2001).
!  An  early version appeared as a Technical Report ENSEEIHT-IRIT,
!  RT/APO/99/2 (1999)  and is available at http://www.enseeiht.fr/apo/ZMUMPS/.
!
!  None of the text from the Copyright notice up to and
!  including this line shall be removed or altered in any way.
!
!     $Id: zmumps_root.h,v 1.7 2003/05/13 13:34:16 jylexcel Exp $
      TYPE ZMUMPS_ROOT_STRUC
        SEQUENCE
        INTEGER*4 MBLOCK, NBLOCK, NPROW, NPCOL
        INTEGER*4 MYROW, MYCOL
        INTEGER*4 ROOT_SIZE, TOT_ROOT_SIZE
        INTEGER*4 :: CNTXT_BLACS
        INTEGER*4, DIMENSION(:), POINTER :: RG2L_ROW
        INTEGER*4, DIMENSION(:), POINTER :: RG2L_COL
        INTEGER*4 , DIMENSION(:), POINTER :: IPIV
        INTEGER*4, DIMENSION( 9 ) :: DESCRIPTOR, DESCB
        LOGICAL yes, gridinit_done
        INTEGER*4 LPIV
!
!      Data for nullspace/QR
!
        COMPLEX*16, DIMENSION(:), POINTER :: QR_TAU
!
!      Givens rotations
!
        INTEGER*4 MAXG, GIND
        COMPLEX*16, DIMENSION(:),POINTER::GROW, GCOS, GSIN
!
!      RRRLU data
!
        INTEGER*4 ELG_MAX,NULL_MAX
        INTEGER*4 ELIND,EUIND,NLUPDATE,NUUPDATE
        INTEGER*4,DIMENSION(:),POINTER::PERM_ROW,PERM_COL
        INTEGER*4,DIMENSION(:),POINTER::ELROW, EUROW, PTREL, PTREU
        COMPLEX*16, DIMENSION(:), POINTER :: ELELG, EUELG, DL
!
      END TYPE ZMUMPS_ROOT_STRUC

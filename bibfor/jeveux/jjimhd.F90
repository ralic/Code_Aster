subroutine jjimhd(idfic, inat, crnom, ngrp, kattr,&
                  iadmi, genri, typei, lt, lonoi)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "jeveux_private.h"
#include "asterc/hdfcld.h"
#include "asterc/hdfclg.h"
#include "asterc/hdfcrg.h"
#include "asterc/hdfopd.h"
#include "asterc/hdfwat.h"
#include "asterc/hdfwsv.h"
#include "asterfort/u2mesk.h"
    integer :: idfic, inat, iadmi, lt, lonoi
    character(len=*) :: crnom, ngrp, genri, typei
    character(len=24) :: kattr(*)
! ----------------------------------------------------------------------
! ROUTINE UTILISATEUR : ECRIT UN SEGMENT DE VALEURS DANS UN FICHIER HDF
!
! IN  IDFIC  : IDENTIFICATEUR DU FICHIER HDF
! IN  INAT   : TYPE DE L'OBJET JEVEUX : 1 OBJET SIMPLE, 2 COLLECTION
!                                       3 OBJET DE COLLECTION
! IN  CRNOM  : NOM DU DATASET (NOM DE L'OBJET JEVEUX)
! IN  NGRP   : NOM DU GROUPE SOUS LEQUEL LE DATASET EST CREE
! IN  KATTR  : LISTE D'ATTRIBUTS A STOCKER AVEC LE DATASET
! IN  IADMI  : ADRESSE DU PREMIER MOT DU SEGMENT DE VALEUR
! IN  GENRI  : GENRE DE L'OBJET
! IN  TYPEI  : TYPE DE L'OBJET
! IN  LT     : LONGUEUR DU TYPE
! IN  LONOI  : LONGEUR EN ENTIER DU SEGMENT
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: jdocu, jgenr, ji, jitab, jorig, jrnom
    integer :: jtype, kitab, n
!-----------------------------------------------------------------------
    parameter      ( n = 5 )
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
! ----------------------------------------------------------------------
    integer :: ilorep, ideno, ilnom, ilmax, idehc
    parameter      ( ilorep=1,ideno=2,ilnom=3,ilmax=4,idehc=6)
! ----------------------------------------------------------------------
    integer :: lg, iret, iddat, kadm, ladm, idg
    character(len=24) :: nomatr
    parameter      ( nomatr = 'ATTRIBUTS JEVEUX' )
! DEB ------------------------------------------------------------------
    kadm = iadmi
    ladm = iszon(jiszon + kadm - 3)
!
    if (inat .eq. 0) then
        kattr(1)='OBJET SYSTEME'
    else if (inat .eq. 1) then
        kattr(1)='OBJET SIMPLE'
    else if (inat .eq. 2) then
        kattr(1)='OBJ. SYSTEME COLLECTION'
    else if (inat .eq. 3) then
        kattr(1)='OBJET DE COLLECTION'
    else if (inat .eq. -1) then
        kattr(1)='COLLECTION'
    endif
    kattr(3)(3:3)=genri
    if (genri .ne. 'N') then
        if (typei .eq. 'S') then
            ji = 1 + (jiszon +kadm - 1) *lois+ladm
            lg=lonoi/(lor8/2)
            iret = hdfwsv (idfic,ngrp,crnom,typei,lt,k1zon(ji),lg)
            kattr(3)(4:24)=' I4'
            kattr(4)='INTEGER*4'
        else if (typei .eq. 'I') then
            ji = 1 + (jiszon +kadm - 1) *lois+ladm
            lg=lonoi/lois
            iret = hdfwsv (idfic,ngrp,crnom,typei,lt,k1zon(ji),lg)
            kattr(3)(4:24)=' I'
            kattr(4)='INTEGER'
        else if (typei .eq. 'R') then
            ji = 1 + (jiszon +kadm - 1) *lois+ladm
            lg=lonoi/lor8
            iret = hdfwsv (idfic,ngrp,crnom,typei,lt,k1zon(ji),lg)
            kattr(3)(4:24)=' R'
            kattr(4)='REAL*8'
        else if (typei .eq. 'C') then
            ji = 1 + (jiszon +kadm - 1) *lois+ladm
            lg=lonoi/lor8
            iret = hdfwsv (idfic,ngrp,crnom,typei,lt,k1zon(ji),lg)
            kattr(3)(4:24)=' C'
            kattr(4)='COMPLEX*16'
        else if (typei .eq. 'L') then
            ji = 1 + ( jiszon + kadm - 1) * lois + ladm
            lg=lonoi/lols
            iret = hdfwsv (idfic,ngrp,crnom,typei,lt,k1zon(ji),lg)
            kattr(3)(4:24)=' L'
            kattr(4)='LOGICAL'
        else if (typei .eq. 'K') then
            ji = 1 + ( jiszon + kadm - 1) * lois + ladm
            lg=lonoi/lt
            iret = hdfwsv (idfic,ngrp,crnom,typei,lt,k1zon(ji),lg)
            kattr(3)(4:24)=' K'
            kattr(4)='CHARACTER*'
            if (lt .gt. 9) then
                write(kattr(3)(6:7),'(I2)') lt
                write(kattr(4)(11:12),'(I2)') lt
            else
                write(kattr(3)(6:6),'(I1)') lt
                write(kattr(4)(11:11),'(I1)') lt
            endif
        else
            call u2mesk('F', 'JEVEUX1_43', 1, typei)
        endif
        iddat = hdfopd (idfic,ngrp,crnom)
        iret = hdfwat (iddat,nomatr,5,kattr)
        if (iret .lt. 0) then
            call u2mesk('A', 'JEVEUX1_48', 1, crnom)
        endif
        iret = hdfcld (iddat)
    else
        idg = hdfcrg (idfic,ngrp,crnom)
        jitab = jiszon + kadm - 1
        kitab = jk1zon + (kadm - 1) * lois
        ji = kitab+iszon(jitab+ideno)+ 1
        lg = iszon(jitab+ilmax)
        lt = iszon(jitab+ilnom)
        kattr(3)(2:24)=' N K'
        kattr(4)='CHARACTER*'
        if (lt .gt. 9) then
            write(kattr(3)(6:7),'(I2)') lt
            write(kattr(4)(11:12),'(I2)') lt
        else
            write(kattr(3)(6:6),'(I1)') lt
            write(kattr(4)(11:11),'(I1)') lt
        endif
        iret = hdfwat (idg,nomatr,5,kattr)
        if (iret .lt. 0) then
            call u2mesk('A', 'JEVEUX1_48', 1, crnom)
        endif
        iret = hdfwsv (idfic,crnom,'T_NOM',typei,lt,k1zon(ji),lg)
        typei = 'I'
        ji = 1 + (jiszon +kadm - 1) *lois+ladm
        lg = iszon(jitab + ilorep)+ idehc
        lt = lois
        iret = hdfwsv (idfic,crnom,'T_HCOD',typei,lt,k1zon(ji),lg)
        iret = hdfclg (idg)
    endif
! FIN ------------------------------------------------------------------
end subroutine

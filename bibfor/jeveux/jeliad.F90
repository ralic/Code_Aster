subroutine jeliad(clas, numr, nboct)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
! ROUTINE UTILISATEUR RENVOYANT LE NUMERO DE L'ENREGISTREMENT CONTENANT
!         L'OBJET SYSTEME $$RNOM DANS LE FICHIER D'ACCES DIRECT ASSOCIE
! IN  : CLAS NOM DE LA CLASSE ASSOCIEE ('G','V', ...)
! OUT : NUMR NUMERO DE L'ENREGISTREMENT
! OUT : NBOCT NOMBRE D'OCTETS STOCKES AVANT L'ENREGISTREMENT CONTENANT
!       $$RNOM
!
! ----------------------------------------------------------------------
    implicit none
#include "jeveux_private.h"
#include "asterfort/assert.h"
    character(len=*) :: clas
    integer :: numr, nboct, n
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: jcara, jdate, jhcod, jiadd, jiadm, jlong, jlono
    integer :: jltyp, jluti, jmarq
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
! ----------------------------------------------------------------------
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
!
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
! ----------------------------------------------------------------------
    character(len=1) :: kclas
    integer :: ic, lgbl
! DEB ------------------------------------------------------------------
    kclas = clas
    ic = index ( classe , kclas )
    ASSERT(ic .ne. 0)
!
!     L'OBJET $$RNOM CORRESPOND A L'INDICE 7
!
    numr = iadd(jiadd(ic)+2*7-1)
    lgbl = 1024*longbl(ic)*lois
    nboct = lgbl*(numr-1)
!
! FIN ------------------------------------------------------------------
end subroutine

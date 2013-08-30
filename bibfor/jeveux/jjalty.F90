subroutine jjalty(typei, ltypi, cel, inatb, jctab)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=C1002,W0405
    implicit none
#include "jeveux.h"
#include "asterfort/jxveuo.h"
    integer :: ltypi, inatb, jctab
    character(len=*) :: typei, cel
!-----------------------------------------------------------------------
! ALLOUE LE SEGMENT DE VALEURS EN MEMOIRE ET LE POSITIONNE EN
! FONCTION DU TYPE ASSOCIE
!
! IN   TYPEI  : TYPE DE L'OBJET
! IN   LTYPI  : LONGUEUR DU TYPE
! IN   CEL    : 'L' OU 'E'
! IN   INATB  : TYPE D'OBJET 1:OS 2:CO 3:OC
! OUT  JCTAB  : ADRESSE PAR RAPPORT AU COMMUN DE REFERENCE
!
!-----------------------------------------------------------------------
    integer :: izr(1), izc(1), izl(1), izk8(1), izk16(1), izk24(1),&
               izk32(1), izk80(1), izi4(1)
    equivalence    (izr,zr),(izc,zc),(izl,zl),(izk8,zk8),(izk16,zk16),&
                   (izk24,zk24),(izk32,zk32),(izk80,zk80),(izi4,zi4)
! DEB ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    jctab = 0
    if (typei .eq. 'I') then
        call jxveuo(cel, zi, inatb, jctab)
    else if (typei .eq. 'S') then
        call jxveuo(cel, izi4, inatb, jctab)
    else if (typei .eq. 'R') then
        call jxveuo(cel, izr, inatb, jctab)
    else if (typei .eq. 'C') then
        call jxveuo(cel, izc, inatb, jctab)
    else if (typei .eq. 'K') then
        if (ltypi .eq. 8) then
            call jxveuo(cel, izk8, inatb, jctab)
        else if (ltypi .eq. 16) then
            call jxveuo(cel, izk16, inatb, jctab)
        else if (ltypi .eq. 24) then
            call jxveuo(cel, izk24, inatb, jctab)
        else if (ltypi .eq. 32) then
            call jxveuo(cel, izk32, inatb, jctab)
        else if (ltypi .eq. 80) then
            call jxveuo(cel, izk80, inatb, jctab)
        else
            call jxveuo(cel, izk8, inatb, jctab)
        endif
    else if (typei .eq. 'L') then
        call jxveuo(cel, izl, inatb, jctab)
    endif
! FIN ------------------------------------------------------------------
end subroutine

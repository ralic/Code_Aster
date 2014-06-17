subroutine mnlcdl(imat, numedd, xcdl, nd,lcine)
    implicit none
!
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
! ----------------------------------------------------------------------
!
!       MODE NON LINEAIRE - GESTION DES CONDITIONS AUX LIMITES
!           POUR LES MATRICES DE MASSE ET DE RAIDEUR
!       -         -         ---
! ----------------------------------------------------------------------
!
! IN  IMAT   : I(2) : DESCRIPTEUR DES MATRICES :
!                       - IMAT(1) => MATRICE DE RAIDEUR
!                       - IMAT(2) => MATRICE DE MASSE
! IN  NUMEDD : K14  : NUME_DDL DES MATRICES DE MASSE ET RAIDEUR
! OUT XCDL   : K14  : VECTEUR D'INDICE DES DDLS ACTIFS (0) OU NON (1)
! OUT ND     : I    : NOMBRE DE DDLS ACTIFS
! OUT ND     : L    : .TRUE.  SI CDL = AFFE_CHAR_CINE 
!                     .FALSE. SINON  
! ----------------------------------------------------------------------
!
#include "jeveux.h"
!
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    logical :: lcine
    integer :: imat(2), nd
    character(len=14) :: xcdl, numedd
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    character(len=19) :: matk
    integer :: lccid, iind, neq,  k,  j, tcmp, ndlag
    integer, pointer :: ccid(:) => null()
    integer, pointer :: deeq(:) => null()
!
    call jemarq()
! ----------------------------------------------------------------------
! --- VERIFICATION SI CONDITION CINEMATIQUE OU MECANIQUE
! ----------------------------------------------------------------------
! --- RECUPERATION NOM DE LA MATRICE
    matk=zk24(zi(imat(1)+1))(1:19)
! --- EXISTENCE DU CHAMP CCID (CDL CINE) DANS LA MATRICE
    call jeexin(matk//'.CCID', lccid)
! --- CREATION DU VECTEUR INDIQUANT SI ACTIF (0) OU NON(1)
    call jeveuo(xcdl, 'E', iind)
! --- TAILLE GLOBALE DE LA MATRICE DE RAIDEUR (OU DE MASSE)
    neq = zi(imat(1)+2)
!
! --- CAS MATR_ASSE_GENE
    if (lccid.gt.0) then
! --- CAS AFFE_CHAR_CINE
        call jeveuo(matk//'.CCID', 'L', vi=ccid)
        do 10 k = 1, neq
            zi(iind-1+k)=ccid(k)
10      continue
        nd=neq-ccid(neq+1)
        lcine = .true.
    else
! --- CAS AFFE_CHAR_MECA
        lcine = .false.
        call jeveuo(numedd//'.NUME.DEEQ', 'L', vi=deeq)
        do 20 k = 1, neq
            if (deeq(2*(k-1)+2) .gt. 0) then
                zi(iind-1+k)=0
            else if (deeq(2*(k-1)+2).lt.0) then
                zi(iind-1+k)=1
                j=1
                tcmp=-deeq(2*(k-1)+2)
21              continue
                if (deeq(2*(j-1)+1) .ne. deeq(2*(k-1)+1) .or.&
                    deeq(2*(j-1)+2) .ne. tcmp) then
                    j=j+1
                    goto 21
                endif
                zi(iind-1+j)=1
            endif
20      continue
        ndlag=0
        do 30 k = 1, neq
            if (zi(iind-1+k) .eq. 1) then
                ndlag=ndlag+1
            endif
30      continue
        nd=neq-ndlag
    endif
!
    call jedema()
!
end subroutine

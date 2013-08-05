subroutine mmssfr(defico, izone, posmae, ndexfr)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmex.h"
#include "asterfort/cfnumn.h"
#include "asterfort/cfposn.h"
#include "asterfort/iscode.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mminfi.h"
    character(len=24) :: defico
    integer :: posmae
    integer :: ndexfr, izone
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT - UTILITAIRE)
!
! INDICATEUR QU'UNE MAILLE ESCLAVE CONTIENT DES NOEUDS
! EXCLUS PAR SANS_GROUP_NO_FR
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
! IN  IZONE  : NUMEOR DE LA ZONE DE CONTACT
! IN  POSMAE : NUMERO DE LA MAILLE ESCLAVE
! OUT NDEXFR : ENTIER CODE DES NOEUDS EXCLUS
!
!
!
!
    integer :: numno
    integer :: nnomai, posnno(9), numnno(9)
    integer :: suppok, ino
    integer :: ndimg, ndirex
    integer :: ndexcl(10), nbexfr
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nbexfr = 0
    ndexfr = 0
    do 10 ino = 1, 10
        ndexcl(ino) = 0
10  end do
!
! --- NUMEROS DES NOEUDS DE LA MAILLE DANS SD CONTACT
!
    call cfposn(defico, posmae, posnno, nnomai)
    call cfnumn(defico, nnomai, posnno, numnno)
    ASSERT(nnomai.le.9)
!
! --- REPERAGE SI LE NOEUD EST UN NOEUD A EXCLURE
!
    do 50 ino = 1, nnomai
        numno = numnno(ino)
        call cfmmex(defico, 'FROT', izone, numno, suppok)
        if (suppok .eq. 1) then
            nbexfr = nbexfr + 1
            ndexcl(ino) = 1
        else
            ndexcl(ino) = 0
        endif
50  end do
!
! --- CODAGE
!
    if (nbexfr .ne. 0) then
!
! ----- NOMBRE DE DIRECTIONS A EXCLURE
!
        ndimg = cfdisi(defico,'NDIM')
        ndirex = mminfi(defico,'EXCL_DIR',izone)
        if (ndimg .eq. 2) then
            if (ndirex .gt. 0) then
                ndexcl(10) = 1
            else
                ASSERT(.false.)
            endif
        else if (ndimg.eq.3) then
            if (ndirex .eq. 1) then
                ndexcl(10) = 0
            else if (ndirex.eq.2) then
                ndexcl(10) = 1
            else
                ASSERT(.false.)
            endif
        else
            ASSERT(.false.)
        endif
        call iscode(ndexcl, ndexfr, 10)
    endif
!
    call jedema()
end subroutine

subroutine cfgcpc(resoco, matass, solveu, neq, nbliai,&
                  precon, tole, premax, epsi)
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
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfpcdi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/r8inir.h"
#include "blas/dcopy.h"
    character(len=24) :: resoco
    character(len=16) :: precon
    integer :: neq, nbliai
    character(len=19) :: matass, solveu
    integer :: premax
    real(kind=8) :: tole, epsi
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (RESOLUTION - GCP)
!
! PRECONDITIONNEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  PRECON : TYPE DE PRECONDITIONNEMENT
!              'DIRICHLET'
!              'SANS'
! IN  TOLE   : TOLERANCE POUR DETECTER PRESSION NULLE
! IN  PREMAX : NOMBRE MAXI D'ITERATIONS DU SOLVEUR ITERATIF
! IN  EPSI   : RESI_ABSO
!
!
!
!
    character(len=19) :: sgradp, sgrprp
    integer :: jsgrap, jsgprp
    character(len=24) :: apcoef, apddl, appoin
    integer :: japcoe, japddl, japptr
    character(len=19) :: mu, liac
    integer :: jmu, jliac
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcoef = resoco(1:14)//'.APCOEF'
    apddl = resoco(1:14)//'.APDDL'
    mu = resoco(1:14)//'.MU'
    sgradp = resoco(1:14)//'.SGDP'
    liac = resoco(1:14)//'.LIAC'
    sgrprp = resoco(1:14)//'.SGPP'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(mu, 'L', jmu)
    call jeveuo(sgradp, 'L', jsgrap)
    call jeveuo(liac, 'E', jliac)
    call jeveuo(sgrprp, 'E', jsgprp)
!
! --- INITIALISATION DU PRECONDITIONNEUR
!
    call r8inir(nbliai, 0.d0, zr(jsgprp), 1)
!
! --- PRECONDITIONNEMENT (OU PAS)
!
    if (precon .eq. 'DIRICHLET') then
        call cfpcdi(resoco, neq, nbliai, tole, epsi,&
                    zr(jmu), zr( japcoe), zi(japddl), zi(japptr), zi(jliac),&
                    matass, solveu, premax, zr(jsgrap), zr(jsgprp))
    else if (precon.eq.'SANS') then
        call dcopy(nbliai, zr(jsgrap), 1, zr(jsgprp), 1)
    else
        call assert(.false.)
    endif
!
    call jedema()
!
end subroutine

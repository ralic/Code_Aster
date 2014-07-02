subroutine mbrigi(fami, kpg, imate, rig)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!      CALCUL DE LA MATRICE DE RIGIDITE POUR UN COMPORTEMENT
!                   DE MEMBRANE ANISOTROPE
! ----------------------------------------------------------------------
! IN  FAMI         FAMILLE D'ELEMENT
! IN  KPG          NUMERO DU POINT DE GAUSS
! IN  IMATE        IDENTIFIANT DU MATERIAU
! OUT RIG          MATRICE DE RIGIDITE MEMBRANAIRE
! ----------------------------------------------------------------------
!
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
    integer :: codres(7)
    character(len=4) :: fami
    character(len=8) :: nomres(7)
    character(len=13) :: phenom
    integer :: kpg, imate, codret
    real(kind=8) :: valres(7), rig(3, 3)
!
! --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
    integer :: zi
    common /ivarje/zi(1)
    real(kind=8) :: zr
    common /rvarje/zr(1)
    complex(kind=8) :: zc
    common /cvarje/zc(1)
    aster_logical :: zl
    common /lvarje/zl(1)
    character(len=8) :: zk8
    character(len=16) :: zk16
    character(len=24) :: zk24
    character(len=32) :: zk32
    character(len=80) :: zk80
    common /kvarje/zk8(1),zk16(1),zk24(1),zk32(1),zk80(1)
! --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
!
! - VERIFICATION DU COMPORTEMENT
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, codret)
    if (phenom .ne. 'ELAS_MEMBRANE') ASSERT(.false.)
!
! - RECUPERATION DES COMPOSANTES
!
    nomres(1) = 'M_LLLL'
    nomres(2) = 'M_LLTT'
    nomres(3) = 'M_LLLT'
    nomres(4) = 'M_TTTT'
    nomres(5) = 'M_TTLT'
    nomres(6) = 'M_LTLT'
!
    call rcvalb(fami, kpg, 1, '+', zi(imate),&
                ' ', phenom, 0, ' ', [0.d0],&
                6, nomres, valres, codres, 1)
!
! -  EN CAS DE PROBLEME AVEC LES VARIABLES DE COMMANDES
!
!      CALL RCVALA(ZI(IMATE),' ',PHENOM,
!     &            0,' ',0.D0,6,
!     &            NOMRES,VALRES,CODRES, 1)
!
! - CONSTRUCTION DE LA MATRICE DE RIGIDITE
!
    call r8inir(3*3, 0.d0, rig, 1)
    rig(1,1) = valres(1)
    rig(2,1) = valres(2)
    rig(3,1) = valres(3)
    rig(1,2) = valres(2)
    rig(2,2) = valres(4)
    rig(3,2) = valres(5)
    rig(1,3) = valres(3)
    rig(2,3) = valres(5)
    rig(3,3) = valres(6)
!
end subroutine

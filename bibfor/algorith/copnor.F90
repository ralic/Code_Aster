subroutine copnor(noma, defico, resoco, posmai, ksi1,&
                  ksi2, tau1, tau2)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/apvect.h"
#include "asterfort/assert.h"
#include "asterfort/cfnben.h"
#include "asterfort/cfnumm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmcoor.h"
#include "asterfort/mmmron.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmtann.h"
#include "asterfort/mmtypm.h"
#include "asterfort/normev.h"
    integer :: posmai
    real(kind=8) :: tau1(3), tau2(3)
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    real(kind=8) :: ksi1, ksi2
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
!
! CALCUL DES VECTEURS TANGENTS LOCAUX EN UN POINT
! ON PROCEDE PAR INTERPOLATION LINEAIRE DES VECTEURS TANGENTS
! AUX NOEUDS DE LA MAILLE
!
! ----------------------------------------------------------------------
!
! NB: CONFUSION NOEUD/POINT DE CONTACT CAR LES OPTIONS FONCTIONNENT
! QUE POUR INTEGRATION='NOEUD'
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  POSMAI : MAILLE QUI RECOIT LA PROJECTION
! IN  KSI1   : COORDONNEE X DU POINT PROJETE
! IN  KSI2   : COORDONNEE Y DU POINT PROJETE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! OUT TAU1   : PREMIERE TANGENTE LOCALE AU POINT PROJETE
! OUT TAU2   : SECONDE TANGENTE LOCALE AU POINT ROJETE
!
!
!
!
    real(kind=8) :: zero
    parameter  ( zero   =  0.0d0  )
    character(len=24) :: nomaco
    integer :: jnoma
    character(len=19) :: sdappa
    integer :: jdecno, posno, nummai
    integer :: ino, idim, iret
    integer :: ndim, nno
    real(kind=8) :: vecta1(27), vecta2(27), vecnor(27)
    real(kind=8) :: norm(3), noor
    character(len=8) :: alias
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DE QUELQUES DONNEES
!
    nomaco = defico(1:16)//'.NOMACO'
    call jeveuo(nomaco, 'L', jnoma)
!
! --- LECTURE APPARIEMENT
!
    sdappa = resoco(1:14)//'.APPA'
!
! --- INITIALISATIONS
!
    do idim = 1, 3
        tau1(idim) = zero
        tau2(idim) = zero
        norm(idim) = zero
    end do
!
! --- MAILLE COURANTE
!
    call cfnumm(defico, posmai, nummai)
    call cfnben(defico, posmai, 'CONNEX', nno, jdecno)
    call mmtypm(noma, nummai, nno, alias, ndim)
!
! --- RECUPERATIONS DES TANGENTES AU NOEUD
!
    do ino = 1, nno
        posno = zi(jnoma+jdecno+ino-1)
        call apvect(sdappa, 'APPARI_NOEUD_TAU1', posno, tau1)
        call apvect(sdappa, 'APPARI_NOEUD_TAU2', posno, tau2)
        do idim = 1, 3
            vecta1(3*(ino-1)+idim) = tau1(idim)
            vecta2(3*(ino-1)+idim) = tau2(idim)
        end do
    end do
!
! --- VECTEURS NORMAUX LISSES AUX NOEUDS DE LA MAILLE (DEJA NORMES)
!
    do ino = 1, nno
        call mmnorm(ndim, vecta1(3*(ino-1)+1), vecta2(3*(ino-1)+1), vecnor(3*(ino-1)+1), noor)
    end do
!
! --- NORMALE EN CE POINT PAR INTERPOLATION A PARTIR DES VALEURS NODALES
!
    call mmcoor(alias, nno, ndim, vecnor, ksi1,&
                ksi2, norm)
!
! --- NORMALISATION DE LA NORMALE
!
    call normev(norm, noor)
    if (noor .le. r8prem()) then
        ASSERT(.false.)
    endif
!
! --- RECONSTRUCTION DES TANGENTES
!
    call mmmron(ndim, norm, tau1, tau2)
!
! --- NORMALISATION DES TANGENTES
!
    call mmtann(ndim, tau1, tau2, iret)
    if (iret .ne. 0) then
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine

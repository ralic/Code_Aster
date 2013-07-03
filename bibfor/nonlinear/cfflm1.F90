subroutine cfflm1(resoco, ndim, nesmax, nbliai, nbliac)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/r8inir.h"
#include "blas/daxpy.h"
    character(len=24) :: resoco
    integer :: nbliac, nesmax, ndim, nbliai
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! CONSTRUCTION DE LA MATRICE TANGENTE DE FROTTEMENT - TERME POSITIF
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NESMAX : NOMBRE MAX DE NOEUDS ESCLAVES
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! IN  NBLIAI : NOMBRE DE LIAISONS
!
!
!
!
    integer :: ndlmax
    parameter   (ndlmax = 30)
    integer :: jdecal, nbddl
    real(kind=8) :: xmu
    integer :: iliai, iliac
    character(len=24) :: apddl, appoin, apcofr
    integer :: japddl, japptr, japcof
    character(len=19) :: liac, mu
    integer :: jliac, jmu
    character(len=19) :: fro1
    integer :: jfro11, jfro12
    logical :: liaact
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcofr = resoco(1:14)//'.APCOFR'
    apddl = resoco(1:14)//'.APDDL'
    liac = resoco(1:14)//'.LIAC'
    mu = resoco(1:14)//'.MU'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcofr, 'L', japcof)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(mu, 'L', jmu)
    fro1 = resoco(1:14)//'.FRO1'
!
! --- CALCUL DE LA MATRICE E_T*AaT
!
    do 100 iliai = 1, nbliai
!
! ----- INITIALISATION DES COLONNES
!
        call jeveuo(jexnum(fro1, iliai), 'E', jfro11)
        call r8inir(ndlmax, 0.d0, zr(jfro11), 1)
        if (ndim .eq. 3) then
            call jeveuo(jexnum(fro1, iliai+nbliai), 'E', jfro12)
            call r8inir(ndlmax, 0.d0, zr(jfro12), 1)
        endif
!
! ----- LA LIAISON EST-ELLE ACTIVE ?
!
        liaact = .false.
        do 200 iliac = 1, nbliac
            if (zi(jliac-1+iliac) .eq. iliai) liaact = .true.
200      continue
!
! ----- CALCUL
!
        if (liaact) then
            jdecal = zi(japptr+iliai-1)
            nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
            xmu = zr(jmu+3*nbliai+iliai-1)
            call daxpy(nbddl, xmu, zr(japcof+jdecal), 1, zr(jfro11),&
                       1)
            if (ndim .eq. 3) then
                call daxpy(nbddl, xmu, zr(japcof+jdecal+ndlmax*nesmax), 1, zr(jfro12),&
                           1)
            endif
        endif
!
        call jelibe(jexnum(fro1, iliai))
        if (ndim .eq. 3) then
            call jelibe(jexnum(fro1, iliai+nbliai))
        endif
100  end do
!
    call jedema()
!
end subroutine

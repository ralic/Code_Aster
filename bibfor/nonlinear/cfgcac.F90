subroutine cfgcac(resoco, tole, neq, nbliai, nbliac)
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
    include 'jeveux.h'
    include 'asterfort/calatm.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/r8inir.h'
    character(len=24) :: resoco
    integer :: neq, nbliai, nbliac
    real(kind=8) :: tole
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (RESOLUTION - GCP)
!
! ACTIVATION DES LIAISONS ET CALCUL DE LA FORCE DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  TOLE   : TOLERANCE POUR DETECTER PRESSION NULLE
! OUT NBLIAC : NOMBRE DE LIAISONS ACTIVES
!
!
!
!
    integer :: ifm, niv
    integer :: iliai, jdecal, nbddl
    character(len=24) :: apcoef, apddl, appoin
    integer :: japcoe, japddl, japptr
    character(len=19) :: mu, atmu, liac
    integer :: jmu, jatmu, jliac
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcoef = resoco(1:14)//'.APCOEF'
    apddl = resoco(1:14)//'.APDDL'
    mu = resoco(1:14)//'.MU'
    atmu = resoco(1:14)//'.ATMU'
    liac = resoco(1:14)//'.LIAC'
!
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(mu, 'L', jmu)
    call jeveuo(atmu, 'E', jatmu)
    call jeveuo(liac, 'E', jliac)
!
! --- CALCUL DE ATMU SUR TOUTES LES LIAISONS
!
    call r8inir(neq, 0.d0, zr(jatmu), 1)
    do 120 iliai = 1, nbliai
        jdecal = zi(japptr+iliai-1)
        nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
        call calatm(neq, nbddl, zr(jmu+iliai-1), zr(japcoe+jdecal), zi(japddl+jdecal),&
                    zr(jatmu))
120  end do
!
! --- COMPTE DES LIAISONS ACTIVES ET ACTIVATION
!
    nbliac = 0
    do 130 iliai = 1, nbliai
        if (zr(jmu+iliai-1) .gt. tole) then
            nbliac = nbliac + 1
            zi(jliac+nbliac-1) = iliai
        endif
130  end do
!
    call jedema()
!
end subroutine

subroutine ndmapp(sddyna, valinc)
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
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ndynkk.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nmchex.h'
    character(len=19) :: valinc(*)
    character(len=19) :: sddyna
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! MISE A JOUR DES CHAMPS MULTI-APPUI
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
!
!
!
!
    logical :: lmuap
    character(len=19) :: depplu, vitplu, accplu
    character(len=19) :: depent, vitent, accent
    integer :: jdepen, jviten, jaccen
    character(len=19) :: depabs, vitabs, accabs
    integer :: jdepab, jvitab, jaccab
    integer :: jdepp, jvitp, jaccp
    integer :: neq, ie
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVEES
!
    lmuap = ndynlo(sddyna,'MULTI_APPUI')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
!
! --- CALCUL DES DEPL/VITE/ACCE ABSOLU EN MULTI-APPUIS
!
    if (lmuap) then
        call ndynkk(sddyna, 'DEPENT', depent)
        call ndynkk(sddyna, 'VITENT', vitent)
        call ndynkk(sddyna, 'ACCENT', accent)
        call ndynkk(sddyna, 'DEPABS', depabs)
        call ndynkk(sddyna, 'VITABS', vitabs)
        call ndynkk(sddyna, 'ACCABS', accabs)
        call jeveuo(depent(1:19)//'.VALE', 'L', jdepen)
        call jeveuo(vitent(1:19)//'.VALE', 'L', jviten)
        call jeveuo(accent(1:19)//'.VALE', 'L', jaccen)
        call jeveuo(depplu(1:19)//'.VALE', 'L', jdepp)
        call jeveuo(vitplu(1:19)//'.VALE', 'L', jvitp)
        call jeveuo(accplu(1:19)//'.VALE', 'L', jaccp)
        call jeveuo(depabs(1:19)//'.VALE', 'E', jdepab)
        call jeveuo(vitabs(1:19)//'.VALE', 'E', jvitab)
        call jeveuo(accabs(1:19)//'.VALE', 'E', jaccab)
        call jelira(depent(1:19)//'.VALE', 'LONMAX', neq, k8bid)
        do 20 ie = 1, neq
            zr(jdepab+ie-1) = zr(jdepen+ie-1) + zr(jdepp+ie-1)
            zr(jvitab+ie-1) = zr(jviten+ie-1) + zr(jvitp+ie-1)
            zr(jaccab+ie-1) = zr(jaccen+ie-1) + zr(jaccp+ie-1)
20      continue
    endif
!
    call jedema()
!
end subroutine

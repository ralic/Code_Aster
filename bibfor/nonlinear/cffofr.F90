subroutine cffofr(numedd, resoco, cnfofr)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: resoco
    character(len=24) :: numedd
    character(len=24) :: cnfofr
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISRETE - CALCUL)
!
! CALCUL DU VECTEUR ASSEMBLE DES FORCES DE FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUME_DDL DE LA MATRICE
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! OUT CNFOFR : VECT_ASSE DES FORCES DE FROTTEMENT
!
!
!
!
    integer :: neq, iret, iconta, i
    integer :: jafmu, jcnfr
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
    call jeveuo(cnfofr(1:19)//'.VALE', 'E', jcnfr)
!
! --- CALCUL DU VECT_ASSE
!
    call jeexin(resoco(1:14)//'.AFMU', iconta)
    if (iconta .ne. 0) then
        call jeveuo(resoco(1:14)//'.AFMU', 'L', jafmu)
        do 10 i = 1, neq
            zr(jcnfr+i-1) = zr(jafmu+i-1)
10      continue
    endif
!
    call jedema()
end subroutine

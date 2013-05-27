subroutine nmrecz(numedd, cndiri, cnfint, cnfext, ddepla,&
                  fonc)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILLT BE USEFUL, BUT
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
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    real(kind=8) :: fonc
    character(len=24) :: numedd
    character(len=19) :: cndiri, cnfint, cnfext, ddepla
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (RECHERCHE LINEAIRE)
!
! CALCUL DE LA FONCTION POUR LA RECHERCHE LINEAIRE
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NOM DU NUME_DDL
! IN  CNDIRI : VECT_ASSE REACTIONS D'APPUI
! IN  CNFINT : VECT_ASSE FORCES INTERIEURES
! IN  CNFEXT : VECT_ASSE FORCES EXTERIEURES
! IN  DDEPLA : INCREMENT DE DEPLACEMENT
! OUT FONC   : VALEUR DE LA FONCTION
!
!
!
!
    character(len=8) :: k8bid
    integer :: ieq, neq, iret
    integer :: jfext, jfint, jdiri, jddepl
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
!
! --- ACCES OBJETS
!
    call jeveuo(cnfext(1:19)//'.VALE', 'L', jfext)
    call jeveuo(cnfint(1:19)//'.VALE', 'L', jfint)
    call jeveuo(cndiri(1:19)//'.VALE', 'L', jdiri)
    call jeveuo(ddepla(1:19)//'.VALE', 'L', jddepl)
!
    fonc = 0.d0
    do 10 ieq = 1, neq
        fonc = fonc + zr(jddepl+ieq-1) * (zr(jfint+ieq-1)+ zr(jdiri+ ieq-1)- zr(jfext+ieq-1))
10  end do
!
    call jedema()
end subroutine

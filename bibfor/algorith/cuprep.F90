subroutine cuprep(mailla, neq, deficu, resocu, deptot,&
                  inst)
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
#include "asterfort/caladu.h"
#include "asterfort/cudisi.h"
#include "asterfort/cusign.h"
#include "asterfort/fointe.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: mailla
    integer :: neq
    character(len=24) :: deficu, resocu
    character(len=19) :: deptot
    real(kind=8) :: inst
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATERALE (RESOLUTION)
!
! CETTE ROUTINE PREPARE LE CAS DES RELATIONS UNILATERALES QUI
! NE SONT PAS DES DEPLACEMENTS
!
! ----------------------------------------------------------------------
!
!
! - CALCUL DU "JEU" (MEMBRE DE DROITE)
! - CALCUL DES COEFFICIENTS DE LA RELATION LINEAIRE (MEMBRE DE GAUCHE)
!
! IN  NEQ    : NOMBRE D'EQUATIONS DU SYSTEME
! IN  DEFICU : SD DE DEFINITION (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCU : SD DE TRAITEMENT NUMERIQUE
! IN  DEPTOT : DEPLACEMENT TOTAL OBTENU A L'ISSUE DE L'ITERATION
!               DE NEWTON PRECEDENTE
! IN  INST   : INSTANT COURANT
!
!
!
!
!
    integer :: icmp, iret
    real(kind=8) :: coef, val, sign
    character(len=24) :: cmpgcu, coegcu, coedcu
    integer :: jcmpg, jcoefg, jcoefd
    character(len=24) :: apcoef, apjeu, poinoe, apddl, noeucu
    integer :: japcoe, japjeu, jpoi, japddl, jnoeu
    integer :: nnocu, ncmpg, inoe, nbddl, jdecal
    integer :: ifm, niv, numnoe
    character(len=8) :: lispar(4)
    real(kind=8) :: valpar(4)
    real(kind=8), pointer :: coor(:) => null()
    real(kind=8), pointer :: depp(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- LECTURE DES SD
!
    apcoef = resocu(1:14)//'.APCOEF'
    apjeu = resocu(1:14)//'.APJEU'
    apddl = resocu(1:14)//'.APDDL'
    noeucu = deficu(1:16)//'.LISNOE'
    call jeveuo(apjeu, 'E', japjeu)
    call jeveuo(apcoef, 'E', japcoe)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(noeucu, 'L', jnoeu)
!
! --- NOMBRE TOTAL DE DDLS ET NOMBRE TOTAL DE NOEUDS
!
    nnocu = cudisi(deficu,'NNOCU')
    ncmpg = cudisi(deficu,'NCMPG')
!
! --- EVALUATION DU MEMBRE DE DROITE (PSEUDO-JEU)
!
    coedcu = deficu(1:16)//'.COEFD'
    call jeveuo(coedcu, 'L', jcoefd)
    call jeveuo(mailla//'.COORDO    .VALE', 'L', vr=coor)
    lispar(1) = 'INST'
    lispar(2) = 'X'
    lispar(3) = 'Y'
    lispar(4) = 'Z'
    do 10 inoe = 1, nnocu
        numnoe = zi(jnoeu-1+inoe)
        valpar(1) = inst
        valpar(2) = coor(1+3*(numnoe-1))
        valpar(3) = coor(1+3*(numnoe-1)+1)
        valpar(4) = coor(1+3*(numnoe-1)+2)
        call fointe('F', zk8(jcoefd-1+inoe), 4, lispar, valpar,&
                    coef, iret)
        zr(japjeu+inoe-1) = coef
10  end do
!
! --- EVALUATION DU MEMBRE DE GAUCHE (COEFFICIENTS DE LA REL. LIN.)
!
    coegcu = deficu(1:16)//'.COEFG'
    call jeveuo(coegcu, 'L', jcoefg)
    cmpgcu = deficu(1:16)//'.CMPGCU'
    call jeveuo(cmpgcu, 'L', jcmpg)
!
    do 20 icmp = 1, ncmpg
        call cusign(jcmpg, icmp, sign)
        call fointe('F', zk8(jcoefg-1+icmp), 1, ['INST'], [inst],&
                    coef, iret)
        zr(japcoe+icmp) = sign*coef
20  end do
!
! --- CALCUL DE -A.DEPTOT ET RANGEMENT DANS APJEU
!
    poinoe = deficu(1:16)//'.POINOE'
    call jeveuo(poinoe, 'L', jpoi)
    call jeveuo(deptot(1:19)//'.VALE', 'E', vr=depp)
!
    do 30 inoe = 1, nnocu
        jdecal = zi(jpoi+inoe-1)
        nbddl = zi(jpoi+inoe) - zi(jpoi+inoe-1)
        call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+jdecal), depp,&
                    val)
        zr(japjeu+inoe-1) = zr(japjeu+inoe-1) - val
30  end do
!
! ----------------------------------------------------------------------
    call jedema()
!
end subroutine

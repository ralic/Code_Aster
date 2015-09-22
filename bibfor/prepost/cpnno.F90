subroutine cpnno(main,numa,coor,inc,nbno,nomnoe)
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
!
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/reerel.h"
#include "asterfort/assert.h"
!
    integer :: nbno, inc, numa
    real(kind=8) :: coor(3, *)
    character(len=8) :: main
    character(len=24) :: nomnoe
    

!
!
! ----------------------------------------------------------------------
!         CREATION DU NOEUD INTERNE SUPPLEMENTAIRE
! ----------------------------------------------------------------------
! IN        NBNO    NOMBRE DE NOEUDS DU MAILLAGE INITIAL
! IN        NUMA    NUMERO DE LA MAILLE COURANTE
! IN/JXVAR  NOMNOE  REPERTOIRE DE NOMS DES NOEUDS
! VAR       COOR    COORDONNEES DES NOEUDS
! ----------------------------------------------------------------------
!
!
    integer :: lino(6), jtab, lgnd, iret, nbso
    integer :: inc1, inc2, aux
    real(kind=8) ::xe(3), xp(3), tabar(6*3), tole
!
    character(len=8) :: nomnd,eletyp, mailut
    character(len=24) :: valk
    character(len=6) :: knume
! ----------------------------------------------------------------------
!
!
!
    tole=1.d-9

    nbso=3
    eletyp='TR3'
    mailut=main
    call jeveuo(jexnum(mailut//'.CONNEX',numa),'L',jtab)        

! - INSERTION DU NOUVEAU NOEUD
!
!      NOM DU NOEUD CREE
     call codent(nbno+inc, 'G', knume)
     lgnd = lxlgut(knume)
     if (lgnd+2 .gt. 8) then
         call utmess('F', 'ALGELINE_16')
     endif
     nomnd = 'C' // knume
!
!      DECLARATION DU NOEUD CREE
     call jeexin(jexnom(nomnoe, nomnd), iret)
     if (iret .eq. 0) then
         call jecroc(jexnom(nomnoe, nomnd))
     else
         valk = nomnd
         call utmess('F', 'ALGELINE4_5', sk=valk)
     endif

! --- Preparation de la geometrie
    do  inc1=1, nbso
        lino(inc1)= zi(jtab+inc1-1) 
    end do
    aux=1
    do inc1=1,nbso
        do inc2=1,3
            tabar(aux+inc2-1) =  coor(inc2,lino(inc1))
        end do
        aux=aux+3
    end do 
!
! - CALCUL DES COORDONNEES DU NOUVEAU NOEUD
! --- CENTRE DE GRAVITE
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=1.d0/3.d0
    xe(2)=1.d0/3.d0
    xe(3)=0.d0
    call reerel(eletyp, nbso, 3, tabar, xe, xp)
    coor(1,nbno+inc) = xp(1)
    coor(2,nbno+inc) = xp(2)  
    coor(3,nbno+inc) = xp(3)

!
end subroutine


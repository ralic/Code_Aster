subroutine cpnph20(main,numa,coor,ind,nomnoe)
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
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/reerel.h"
!
    integer, intent(in) :: ind
    integer, intent(in) :: numa
    real(kind=8),intent(out) :: coor(3, *)
    character(len=8), intent(in) :: main
    character(len=24), intent(in) :: nomnoe
!
!
! ----------------------------------------------------------------------
!         CREATION DES DDL SUPPLEMENTAIRES 
!         SUR LA FACE DE LA ZONE DE CONTACT ESCLAVE
!         CAS QUAD 8
! ----------------------------------------------------------------------
! IN        MAIN    MAILLAGE INITIAL
! IN        NUMA    NUMERO DE LA MAILLE COURANTE
! IN        IND     INDICE DU PREMIER NOEUD AJOUTE
! IN/JXVAR  NOMNOE  REPERTOIRE DE NOMS DES NOEUDS
! IN/OUT    COOR    COORDONNEES DES NOEUDS
! ----------------------------------------------------------------------
!
!
    integer :: lino(8), jtab, lgnd, iret
    integer :: inc1, inc2
    real(kind=8) ::xe(3), xp(3), tabar(8*3)
!
    character(len=8) :: nomnd,eletyp
    character(len=24) :: valk
    character(len=6) :: knume
! ----------------------------------------------------------------------
!
    call jemarq()
!
! - INSERTION DES NOUVEAUX NOEUDS
    do inc1=1,12
! ------ NOM DU NOEUD CREE
        call codent(ind+inc1-1, 'G', knume)
        lgnd = lxlgut(knume)
        if (lgnd+2 .gt. 8) then
            call utmess('F', 'ALGELINE_16')
        endif
        nomnd = 'C' // knume
! ------ DECLARATION DU NOEUD CREE
        call jeexin(jexnom(nomnoe, nomnd), iret)
        if (iret .eq. 0) then
            call jecroc(jexnom(nomnoe, nomnd))
        else
            valk = nomnd
            call utmess('F', 'ALGELINE4_5', sk=valk)
        endif
    end do
! --- CALCUL DES COORDONNEES DES NOUVEAUX NOEUDS

    call jeveuo(jexnum(main//'.CONNEX',numa),'L',jtab)  
    do  inc1=1, 8
        lino(inc1)= zi(jtab+inc1-1) 
    end do
    do inc1=1,8
        do inc2=1,3
            tabar((inc1-1)*3+inc2) =  coor(inc2,lino(inc1))      
        end do
    end do 
    eletyp='QU8'
! --- NOEUD 1 
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=-1.d0/3.d0
    xe(2)=-1.d0/3.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind) = xp(1)
    coor(2,ind) = xp(2)
    coor(3,ind) = xp(3)
! --- NOEUD 2 
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=1.d0/3.d0
    xe(2)=-1.d0/3.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind+1) = xp(1)
    coor(2,ind+1) = xp(2)
    coor(3,ind+1) = xp(3)
! --- NOEUD 3 
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=1.d0/3.d0
    xe(2)=1.d0/3.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind+2) = xp(1)
    coor(2,ind+2) = xp(2)
    coor(3,ind+2) = xp(3)
! --- NOEUD 4
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=-1.d0/3.d0
    xe(2)=1.d0/3.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind+3) = xp(1)
    coor(2,ind+3) = xp(2)  
    coor(3,ind+3) = xp(3)
!
! --- NOEUD 5 
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=0.d0
    xe(2)=-1.d0/3.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind+4) = xp(1)
    coor(2,ind+4) = xp(2)
    coor(3,ind+4) = xp(3)
! --- NOEUD 6 
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=1.d0/3.d0
    xe(2)=0.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind+5) = xp(1)
    coor(2,ind+5) = xp(2)
    coor(3,ind+5) = xp(3)
! --- NOEUD 7 
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=0.d0
    xe(2)=1.d0/3.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind+6) = xp(1)
    coor(2,ind+6) = xp(2)
    coor(3,ind+6) = xp(3)
! --- NOEUD 8
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=-1.d0/3.d0
    xe(2)=0.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind+7) = xp(1)
    coor(2,ind+7) = xp(2)  
    coor(3,ind+7) = xp(3)
!
! --- NOEUD 1b 
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=-4.d0/6.d0
    xe(2)=-4.d0/6.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind+8) = xp(1)
    coor(2,ind+8) = xp(2)
    coor(3,ind+8) = xp(3)
! --- NOEUD 2b 
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=4.d0/6.d0
    xe(2)=-4.d0/6.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind+9) = xp(1)
    coor(2,ind+9) = xp(2)
    coor(3,ind+9) = xp(3)
! --- NOEUD 3b 
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=4.d0/6.d0
    xe(2)=4.d0/6.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind+10) = xp(1)
    coor(2,ind+10) = xp(2)
    coor(3,ind+10) = xp(3)
! --- NOEUD 4b
    xp(1)=0.d0
    xp(2)=0.d0
    xp(3)=0.d0
    xe(1)=-4.d0/6.d0
    xe(2)=4.d0/6.d0
    xe(3)=0.d0
    call reerel(eletyp, 8, 3, tabar, xe, xp)
    coor(1,ind+11) = xp(1)
    coor(2,ind+11) = xp(2)  
    coor(3,ind+11) = xp(3)
!
    call jedema()
end subroutine

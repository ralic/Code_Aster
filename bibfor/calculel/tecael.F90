subroutine tecael(iadzi, iazk24, noms)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/assert.h"
    integer, intent(out) :: iadzi
    integer, intent(out) :: iazk24
    integer, intent(in), optional :: noms
!----------------------------------------------------------------------
! entree:
!     noms=1/0
!        /1  (defaut) : On remplit v(3),...,v(3+nbno)
!        /0           : On ne remplit pas v(3),...,v(3+nbno) : noms de la maille et de ses noeuds
!                       => C'est moins couteux en CPU.
!
! sorties:
!     iadzi est l'adresse d'un vecteur d'entiers  contenant :
!     dim(v)=4+nbno
!        v(1) : numero de la maille
!        v(2) : nombre de noeuds de la maille (nbno)
!        v(2+   1) : numero du 1er noeud de la maille
!        ...
!        v(2+nbno) : numero du der noeud de la maille
!        v(2+nbno +1) : numero du grel
!        v(2+nbno +2) : numero de l'element dans le grel
!
!     iazk24 est l'adresse d'un vecteur de k24 contenant :
!     dim(v)=8+nbno
!        v(1) : nom du maillage       (k8)
!        v(2) : nom du ligrel         (k19)
!        v(3) : nom de la maille      (k8)
!        v(3  +1) : nom du 1er noeud de la maille (k8)
!        ...
!        v(3+nbno) : nom du der noeud de la maille (k8)
!        v(3+nbno+1): type_element(k16)
!        v(3+nbno+2): option que l'on calcule (k16)
!        v(3+nbno+3): type_maille associe au type_element(k8)
!
!    remarque :
!   si la maille est tardive son nom est conventionellement : ' '
!   si un noeud est tardif   son nom est conventionellement : ' '
!----------------------------------------------------------------------
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    common /caii08/iel
    common /caii10/icaeli,icaelk
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
!
    character(len=16) :: option, nomte, nomtm
    common /cakk01/option,nomte,nomtm
    integer :: ialiel, iamaco, iamsco, icaeli, icaelk, iel, illiel, ilmaco
    integer :: ilmsco, ima, ino, nno, nuno, noms2
!
    character(len=8) :: ma, nomma, nomno
!----------------------------------------------------------------------

    if (present(noms)) then
        noms2=noms
    else
        noms2=1
    endif
    ASSERT(noms2.eq.0 .or. noms2.eq.1)


!   -- recuperation du numero de la maille et du nombre de noeuds :
!   ---------------------------------------------------------------
    ma = zk24(icaelk-1+1)(1:8)
!
    ima = zi(ialiel-1+zi(illiel+igr-1)+iel-1)
    if (ima .gt. 0) then
        nno = zi(ilmaco-1+ima+1) - zi(ilmaco-1+ima)
        zi(icaeli-1+1) = ima
        zi(icaeli-1+2) = nno
        if (noms2.eq.1) then
            call jenuno(jexnum(ma//'.NOMMAI', ima), nomma)
            zk24(icaelk-1+3) = nomma
        else
            zk24(icaelk-1+3) = ' '
        endif
    else
        nno = zi(ilmsco-1-ima+1) - zi(ilmsco-1-ima) - 1
        zi(icaeli-1+1) = ima
        zi(icaeli-1+2) = nno
        zk24(icaelk-1+3) = ' '
    endif
!
    zk24(icaelk-1+3+nno+1) = nomte
    zk24(icaelk-1+3+nno+2) = option
    zk24(icaelk-1+3+nno+3) = nomtm


!   -- recuperation des numeros globaux des noeuds :
!   -------------------------------------------------
    do 10,ino = 1,nno
    if (ima .gt. 0) then
        nuno = zi(iamaco-1+zi(ilmaco+ima-1)+ino-1)
    else
        nuno = zi(iamsco-1+zi(ilmsco-ima-1)+ino-1)
    endif
    zi(icaeli-1+2+ino) = nuno
!
    if ((noms2.eq.1) .and. (nuno .gt. 0)) then
        call jenuno(jexnum(ma//'.NOMNOE', nuno), nomno)
        zk24(icaelk-1+3+ino) = nomno
    else
        zk24(icaelk-1+3+ino) = ' '
    endif
    10 end do
!
    zi(icaeli-1+2+nno+1) = igr
    zi(icaeli-1+2+nno+2) = iel
!
    iadzi = icaeli
    iazk24 = icaelk
!
!
end subroutine

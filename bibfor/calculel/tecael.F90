subroutine tecael(iadzi, iazk24)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/jenuno.h'
    include 'asterfort/jexnum.h'
    integer :: iadzi, iazk24
!     -----------------------------------------------------------------
!     SORTIES:
!     IADZI EST L'ADRESSE D'UN VECTEUR D'ENTIERS  CONTENANT :
!     DIM(V)=4+NBNO
!        V(1) : NUMERO DE LA MAILLE
!        V(2) : NOMBRE DE NOEUDS DE LA MAILLE (NBNO)
!        V(2+   1) : NUMERO DU 1ER NOEUD DE LA MAILLE
!        ...
!        V(2+NBNO) : NUMERO DU DER NOEUD DE LA MAILLE
!        V(2+NBNO +1) : NUMERO DU GREL
!        V(2+NBNO +2) : NUMERO DE L'ELEMENT DANS LE GREL
!
!     IAZK24 EST L'ADRESSE D'UN VECTEUR DE K24 CONTENANT :
!     DIM(V)=8+NBNO
!        V(1) : NOM DU MAILLAGE       (K8)
!        V(2) : NOM DU LIGREL         (K19)
!        V(3) : NOM DE LA MAILLE      (K8)
!        V(3  +1) : NOM DU 1ER NOEUD DE LA MAILLE (K8)
!        ...
!        V(3+NBNO) : NOM DU DER NOEUD DE LA MAILLE (K8)
!        V(3+NBNO+1): TYPE_ELEMENT(K16)
!        V(3+NBNO+2): OPTION QUE L'ON CALCULE (K16)
!        V(3+NBNO+3): TYPE_MAILLE ASSOCIE AU TYPE_ELEMENT(K8)
!        V(3+NBNO+4): PHENOMENE ASSOCIE AU TYPE_ELEMENT(K16)
!        V(3+NBNO+5): NOM DE LA MODELISATION ASSOCIEE AU
!                     TYPE_ELEMENT(K16)
!
!    REMARQUE :
!   SI LA MAILLE EST TARDIVE SON NOM EST CONVENTIONELLEMENT : ' '
!   SI UN NOEUD EST TARDIF   SON NOM EST CONVENTIONELLEMENT : ' '
!
!
!     -----------------------------------------------------------------
!
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    common /caii08/iel
    common /caii10/icaeli,icaelk
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
!
    character(len=16) :: option, nomte, nomtm, pheno, modeli
    common /cakk01/option,nomte,nomtm,pheno,modeli
    integer :: ialiel, iamaco, iamsco, icaeli, icaelk, iel, illiel, ilmaco
    integer :: ilmsco, ima, ino, nno, nuno
!
    character(len=8) :: ma, nomma, nomno
!
!
!     RECUPERATION DU NUMERO DE LA MAILLE ET DU NOMBRE DE NOEUDS :
!     ------------------------------------------------------------
    ma = zk24(icaelk-1+1)
!
    ima = zi(ialiel-1+zi(illiel+igr-1)+iel-1)
    if (ima .gt. 0) then
        nno = zi(ilmaco-1+ima+1) - zi(ilmaco-1+ima)
        zi(icaeli-1+1) = ima
        zi(icaeli-1+2) = nno
        call jenuno(jexnum(ma//'.NOMMAI', ima), nomma)
        zk24(icaelk-1+3) = nomma
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
    zk24(icaelk-1+3+nno+4) = pheno
    zk24(icaelk-1+3+nno+5) = modeli
!
!     RECUPERATION DES NUMEROS GLOBAUX DES NOEUDS :
!     ---------------------------------------------
    do 10,ino = 1,nno
    if (ima .gt. 0) then
        nuno = zi(iamaco-1+zi(ilmaco+ima-1)+ino-1)
    else
        nuno = zi(iamsco-1+zi(ilmsco-ima-1)+ino-1)
    endif
    zi(icaeli-1+2+ino) = nuno
!
    if (nuno .gt. 0) then
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

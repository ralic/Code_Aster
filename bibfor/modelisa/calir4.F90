subroutine calir4(noma, lisrel, nono2, ino2, v1,&
                  jconb1, jcocf1, jconu1, ideca1, jconb2,&
                  jcocf2, jconu2, ideca2)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/afrela.h'
    include 'asterfort/assert.h'
    include 'asterfort/base3n.h'
    include 'asterfort/imprel.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jexnum.h'
    character(len=19) :: lisrel
    character(len=8) :: nono2, noma
    integer :: ino2, jconb1, jcocf1, jconu1, ideca1
    integer :: jconb2, jcocf2, jconu2, ideca2
    real(kind=8) :: v1(3)
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
! BUT : ECRIRE LES RELATIONS LINEAIRES LIANT 2 ROTATIONS D'UN NOEUD DE
!       COQUE AVEC LES TRANSLATIONS DE 2 NOEUDS "MASSIFS"
!       (AFFE_CHAR_MECA/LIAISON_MAIL + TYPE_RACCORD='COQUE_MASSIF'
! ======================================================================
!
    real(kind=8) :: beta
    character(len=2) :: typlag
    character(len=4) :: fonree
    character(len=4) :: typcoe
!
    real(kind=8) :: coefr(165), direct(3*165), coef1, epais
    real(kind=8) :: mat33(3, 3)
    complex(kind=8) :: cbid, betac
    character(len=8) :: kbeta, noeud(165), ddl(165), nono1
    integer :: dimens(165), nbterm
    integer :: n1, n2, ino1, nuno1, k, idec
! ----------------------------------------------------------------------
!
    beta=0.0d0
    betac=(0.0d0,0.0d0)
    kbeta=' '
    typcoe='REEL'
    fonree='REEL'
    typlag='12'
!
    n1=zi(jconb1-1+ino2)
    n2=zi(jconb2-1+ino2)
    nbterm=3*(1+n1+n2)
!     -- L'EPAISSEUR EST LA NORME DE V1 :
    epais=sqrt(v1(1)*v1(1)+v1(2)*v1(2)+v1(3)*v1(3))
    call assert(nbterm.le.165)
!
!
    call base3n(v1, mat33)
!
!
    do 10,k=1,nbterm
    dimens(k)=0
    10 end do
!
!
!     -- CALCUL DES COEFFICIENTS DE LA RELATION DR2=... :
!     ----------------------------------------------------
    noeud(1)=nono2
    noeud(2)=nono2
    noeud(3)=nono2
    ddl(1)='DRX'
    ddl(2)='DRY'
    ddl(3)='DRZ'
    coefr(1)=-1.d0*mat33(1,2)
    coefr(2)=-1.d0*mat33(2,2)
    coefr(3)=-1.d0*mat33(3,2)
!
!     -- NOEUDS DE LA 1ERE MAILLE (CORRE1) :
    do 20,ino1=1,n1
    nuno1=zi(jconu1+ideca1-1+ino1)
    coef1=zr(jcocf1+ideca1-1+ino1)
    call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
    idec=3+3*(ino1-1)
    noeud(idec+1)=nono1
    noeud(idec+2)=nono1
    noeud(idec+3)=nono1
    ddl(idec+1)='DX'
    ddl(idec+2)='DY'
    ddl(idec+3)='DZ'
    coefr(idec+1)=-(coef1*mat33(1,3))/epais
    coefr(idec+2)=-(coef1*mat33(2,3))/epais
    coefr(idec+3)=-(coef1*mat33(3,3))/epais
    20 end do
!
!     -- NOEUDS DE LA 2EME MAILLE (CORRE2) :
    do 30,ino1=1,n2
    nuno1=zi(jconu2+ideca2-1+ino1)
    coef1=zr(jcocf2+ideca2-1+ino1)
    call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
    idec=3+3*(n1+ino1-1)
    noeud(idec+1)=nono1
    noeud(idec+2)=nono1
    noeud(idec+3)=nono1
    ddl(idec+1)='DX'
    ddl(idec+2)='DY'
    ddl(idec+3)='DZ'
    coefr(idec+1)=+(coef1*mat33(1,3))/epais
    coefr(idec+2)=+(coef1*mat33(2,3))/epais
    coefr(idec+3)=+(coef1*mat33(3,3))/epais
    30 end do
!
    call afrela(coefr, cbid, ddl, noeud, dimens,&
                direct, nbterm, beta, betac, kbeta,&
                typcoe, fonree, typlag, 1.d-6, lisrel)
    call imprel('LIAISON_MAIL-COQUE_MASSIF', nbterm, coefr, ddl, noeud,&
                beta)
!
!
!
!     -- CALCUL DES COEFFICIENTS DE LA RELATION DR3=... :
!     ----------------------------------------------------
    noeud(1)=nono2
    noeud(2)=nono2
    noeud(3)=nono2
    ddl(1)='DRX'
    ddl(2)='DRY'
    ddl(3)='DRZ'
    coefr(1)=-1.d0*mat33(1,3)
    coefr(2)=-1.d0*mat33(2,3)
    coefr(3)=-1.d0*mat33(3,3)
!
!     -- NOEUDS DE LA 1ERE MAILLE (CORRE1) :
    do 40,ino1=1,n1
    nuno1=zi(jconu1+ideca1-1+ino1)
    coef1=zr(jcocf1+ideca1-1+ino1)
    call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
    idec=3+3*(ino1-1)
    noeud(idec+1)=nono1
    noeud(idec+2)=nono1
    noeud(idec+3)=nono1
    ddl(idec+1)='DX'
    ddl(idec+2)='DY'
    ddl(idec+3)='DZ'
    coefr(idec+1)=+(coef1*mat33(1,2))/epais
    coefr(idec+2)=+(coef1*mat33(2,2))/epais
    coefr(idec+3)=+(coef1*mat33(3,2))/epais
    40 end do
!
!     -- NOEUDS DE LA 2EME MAILLE (CORRE2) :
    do 50,ino1=1,n2
    nuno1=zi(jconu2+ideca2-1+ino1)
    coef1=zr(jcocf2+ideca2-1+ino1)
    call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
    idec=3+3*(n1+ino1-1)
    noeud(idec+1)=nono1
    noeud(idec+2)=nono1
    noeud(idec+3)=nono1
    ddl(idec+1)='DX'
    ddl(idec+2)='DY'
    ddl(idec+3)='DZ'
    coefr(idec+1)=-(coef1*mat33(1,2))/epais
    coefr(idec+2)=-(coef1*mat33(2,2))/epais
    coefr(idec+3)=-(coef1*mat33(3,2))/epais
    50 end do
!
    call afrela(coefr, cbid, ddl, noeud, dimens,&
                direct, nbterm, beta, betac, kbeta,&
                typcoe, fonree, typlag, 1.d-6, lisrel)
    call imprel('LIAISON_MAIL-COQUE_MASSIF', nbterm, coefr, ddl, noeud,&
                beta)
!
!
!
end subroutine

subroutine cacina(ndim, nno, npg, lgpg, axi,&
                  grand, compor, geomm, g, iw,&
                  vff, idff, fm, fma, depld,&
                  instm, instp, vim, rp, rpa,&
                  lambp)
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
! TOLE CRP_21 CRS_1404
    implicit none
    include 'asterc/r8miem.h'
    include 'asterfort/dfdmip.h'
    include 'asterfort/lctr2m.h'
    include 'asterfort/matinv.h'
    include 'asterfort/nmepsi.h'
    include 'asterfort/pmat.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    character(len=16) :: compor(*)
    integer :: ndim, nno, g, iw, idff, i, j, npg, lgpg, ivash2
    logical :: axi, grand
    real(kind=8) :: fm(3, 3), fma(3, 3)
    real(kind=8) :: depld(3*27), geomm(3, nno), vff(nno, npg)
    real(kind=8) :: r, rbid, dff(nno, 3), tbid(6)
    real(kind=8) :: instm, instp, vim(lgpg), norm, norm2, a, a2
    real(kind=8) :: b, b2, lambm(3, 3)
    real(kind=8) :: lambmt(3, 3), rp(3, 3)
    real(kind=8) :: lambp(3, 3)
    real(kind=8) :: rpa(3, 3), ind
    real(kind=8) :: lambpa(3, 3), lambam(3, 3)
    real(kind=8) :: vwa(3), inda, norma, norm2a
    real(kind=8) :: aa, a2a, ba, b2a, wwa(3, 3), det
!
! ---------------------------------
!   NDIM    : DIMENSION DE L'ESPACE
!   NNO     : NOMBRE DE NOEUDS DE ELEMENT
!   NPG     : NOMBRE DE POINTS DE GAUSS
!   LGPG    : NOMBRE DE VARIABLE INTERNE POUR UN POINT DE GAUSS
!   AXI     : OUI SI AXISYMETRIQUE NON SINON
!   GRAND   : OUI SI GRANDES DEFORMATIONS NON SINON
!   COMPOR  : COMPORTEMENT
!   GEOMM   : COORDONNEES DES NOEUDS A L INSTANT T-
!   G       : NUMERO DU POINT DE GAUSS COURANT
!   IW      : PTR. POIDS DES POINTS DE GAUSS
!   VFF     : VALEUR  DES FONCTIONS DE FORME
!   IDFF    : PTR. DERIVEE DES FONCTIONS DE FORME ELEMENT DE REF.
!   FM      : TENSEUR DE DEFORMATION ENTRE CONFIGURATIONS INITIALE ET T-
!   FMA     :  TENSEUR DE DEFORMATION ENTRE CONFIGURATION INITIALE ET
!             CONFIGURATION INTERMEDIAIRE
!   DEPLD   : INCREMENT DE DEPLACEMENT ENTRE T- ET T+
!   INSTM   : VALEUR DE INSTANT T-
!   INSTP   : VALEUR DE INSTANT T+
!   VIM     : VARIABLE INTERNE TAIILE DEPEND DU COMPORTEMENT
!   SORTIE  :
!   RP      : LAMBP*TRANSPOSEE(LAMBM)
!   RPA     : LAMBP*INVERSE(LAMBM)
!   LAMBP   :
!
! ----------------------------
!
!     CALCUL DE RP, RPA ET LAMBP
!
! ----------------------------
    real(kind=8) :: fmam(3, 3), id(3, 3), gradu(3, 3)
    real(kind=8) :: prodm(3, 3), hnal(3, 3), hnalt(3, 3)
    real(kind=8) :: w(3, 3), wa(3, 3), vw(3), ww(3, 3)
    real(kind=8) :: rw(3, 3), rwa(3, 3)
    data id/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
!
!     ATTENTION ICI ALPHA=0.5
    ivash2=lgpg-9+1
!
    ind=0.d0
    inda=0.d0
    call lctr2m(3, fma, fmam)
!     CALCUL DE  GRADIENT DE U
    call dfdmip(ndim, nno, axi, geomm, g,&
                iw, vff(1, g), idff, r, rbid,&
                dff)
    call nmepsi(ndim, nno, axi, grand, vff(1, g),&
                r, dff, depld, gradu, tbid)
!
!     PRODUIT DE GRADU*FM*FMAM
    call pmat(3, fm, fmam, prodm)
    call pmat(3, gradu, prodm, hnal)
!
    call lctr2m(3, hnal, hnalt)
    do 70 i = 1, 3
        do 80 j = 1, 3
            w(i,j)=0.5d0*(hnal(i,j)-hnalt(i,j))
            wa(i,j)=0.5d0*0.5d0*(hnal(i,j)-hnalt(i,j))
!       WA EST THETA /2
80      continue
70  end do
!--------------------------------CALCUL DE VECTEUR VW------------
    vw(1)=w(3,2)
    vw(2)=w(1,3)
    vw(3)=w(2,1)
!
    vwa(1)=wa(3,2)
    vwa(2)=wa(1,3)
    vwa(3)=wa(2,1)
!
!--------------------------------CALCUL DE VECTEUR EXP[]------
!
!     CALCUL DE LA NORME DU VECTEUR WW
    do 89 i = 1, 3
        ind= ind + vw(i)*vw(i)
        inda= inda + vwa(i)*vwa(i)
89  end do
    norm = sqrt(ind)
    norm2=norm/2.d0
!
    norma = sqrt(inda)
    norm2a=norma/2.d0
!
!     TEST SI NORM = 0
!     PREPARATION DES TERME DE L ADDITION DE APPROXIMATION EXP
    if (norm .lt. r8miem()) then
        a=1.d0
        a2=1.d0
    else
        a= sin(norm)/norm
        a2=sin(norm2)/norm2
    endif
!
    if (norma .lt. r8miem()) then
        aa=1.d0
        a2a=1.d0
    else
        aa= sin(norma)/norma
        a2a=sin(norm2a)/norm2a
    endif
!
    if (norm .lt. r8miem()) then
        b=1.d0
        b2=1.d0
    else
        b= a*a
        b2=a2*a2
    endif
!
    if (norma .lt. r8miem()) then
        ba=1.d0
        b2a=1.d0
    else
        ba= aa*aa
        b2a=a2a*a2a
    endif
!
    call pmat(3, w, w, ww)
    call pmat(3, wa, wa, wwa)
!
    do 17 i = 1, 3
        do 27 j = 1, 3
            if (i .eq. j) then
                rw(i,j)= 1.d0+a*w(i,j)+0.5d0*b2*ww(i,j)
                rwa(i,j)= 1.d0+aa*wa(i,j)+0.5d0*b2a*wwa(i,j)
            else
                rw(i,j)= a*w(i,j)+0.5d0*b*ww(i,j)
                rwa(i,j)= aa*wa(i,j)+0.5d0*ba*wwa(i,j)
            endif
27      continue
17  end do
!
!-----------------------CALCUL DE LAMBDA_N+1----------
!     RECUPERATION DE LAMBDA_N
!
    call dcopy(9, vim(ivash2), 1, lambm, 1)
    call daxpy(9, 1.d0, id, 1, lambm,&
               1)
!
!     CALCUL DE LAMBDA_N+1
    call pmat(3, rw, lambm, lambp)
!
!     CALCUL DE LAMBDA_N+ALPHA
    call pmat(3, rwa, lambm, lambpa)
!
!----------------CALCUL DE R_N+1=RP ET R~_N+ALPHA=RPA-
!
    call lctr2m(3, lambm, lambmt)
    call pmat(3, lambp, lambmt, rp)
!
    call matinv('S', 3, lambpa, lambam, det)
    call pmat(3, lambp, lambam, rpa)
!
end subroutine

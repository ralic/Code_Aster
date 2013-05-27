subroutine unista(h, ldh, v, ldv, ddlsta,&
                  n, vectp, csta, beta, etat,&
                  ldynfa, ddlexc, redem)
!-----------------------------------------------------------------------
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
!
!-----------------------------------------------------------------------
!
! ROUTINE PRINCIPALE - ALGORITHME D'OPTIMISATION SOUS CONTRAINTES
! POUR LES ETUDES DE STABILITE
!
! ----------------------------------------------------------------------
!
! IN  H        : MATRICE REDUITE
! IN  LDH      : NOMBRE DE COEFFICIENTS DE H
! IN  V        : MATRICE DE CHANGEMENT DE BASE
! IN  LDV      : NOMBRE DE COEFFICIENTS DE V
! IN  DDLSTA   : POSITION DES DDL_STAB
! IN  N        : DIMENSION ESPACE GLOBAL
! IN/OUT VECTP : MODE DE STABILITE
! OUT CSTA     : VALEUR CRITERE DE STABILITE
! IN  BETA     : PLUS GRANDE VALEUR PROPRE NEGATIVE
! IN  ETAT     : =0 TTES LES VP SONT POSITIVES
!                =1 AU MOINS UNE VP EST NEGATIVE
! IN  LDYNFA   : DESCRIPTEUR OPERATEUR TANGENT
! IN  DDLEXC   : POSITION DDL IMPOSES
! IN  REDEM    : NOMBRE REDEMARRAGES METHODE SORENSEN
!
! ----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/mppsta.h'
    include 'asterfort/mrmult.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/wkvect.h'
    include 'blas/dnrm2.h'
    include 'blas/dscal.h'
    integer :: ddlsta(n), ddlexc(n)
    real(kind=8) :: h(ldh, ldh), v(ldv, ldh)
    real(kind=8) :: vectp(ldv)
!
!     %------------------%
!     | SCALAR ARGUMENTS |
!     %------------------%
!
    integer :: n, ldh, ldv, etat, ldynfa
    integer :: redem
    real(kind=8) :: beta, csta
!
!
!     %------------------------%
!     | LOCAL SCALARS & ARRAYS |
!     %------------------------%
!
    integer :: i, j, iret, indico, proj
    integer :: vectt, xsol, vect2
    integer :: ifm, niv
    integer :: q, b
    real(kind=8) :: gama, det
    real(kind=8) :: vtest, err
    real(kind=8) :: zero, one
    parameter (one = 1.0d+0, zero = 0.0d+0)
    character(len=4) :: cara
!
    call infniv(ifm, niv)
!
    call wkvect('&&UNISTA.VECT.TEM1', 'V V R', ldv, vectt)
    call wkvect('&&UNISTA.VECT.TEM2', 'V V R', ldv, xsol)
    call wkvect('&&UNISTA.VECT.TEM3', 'V V R', ldv, vect2)
    call wkvect('&&UNISTA.VECT.TEM4', 'V V R', ldh*ldh, q)
    call wkvect('&&UNISTA.VECT.TEM5', 'V V R', ldh*ldh, b)
!
    call r8inir(ldv, 0.d0, zr(vectt), 1)
    call r8inir(ldv, 0.d0, zr(xsol), 1)
    call r8inir(ldv, 0.d0, zr(vect2), 1)
    call r8inir(ldh*ldh, 0.d0, zr(q), 1)
    call r8inir(ldh*ldh, 0.d0, zr(b), 1)
!
    indico = 0
    proj = 0
!
    if (etat .eq. 0) then
!
!     1ER APPEL A LA METHODE DES PUISSANCES
!
        call mppsta(h, ldh, v, ldv, ddlsta,&
                    n, zr(vectt), ddlexc, indico, proj)
!
    endif
!
    if (etat .eq. 1) then
!
!     MISE EN PLACE SHIFT POUR VALEUR NEGATIVE
!
        gama = abs(beta)+1.d0
!
        do 10 i = 1, ldh
            do 20 j = 1, ldh
                zr(q+(j-1)*ldh+i-1) = -h(i,j)
20          continue
10      continue
        do 30 i = 1, ldh
            zr(q+(i-1)*(ldh+1)) = gama+zr(q+(i-1)*(ldh+1))
30      continue
!
!     2ND APPEL A LA METHODE DES PUISSANCES
!
        proj = 1
!
        call mppsta(zr(q), ldh, v, ldv, ddlsta,&
                    n, zr(vectt), ddlexc, indico, proj)
!
    endif
!
    err = dnrm2(ldv,zr(vectt),1)
    call dscal(ldv, one/err, zr(vectt), 1)
    call mrmult('ZERO', ldynfa, zr(vectt), zr(xsol), 1,&
                .true.)
!
    vtest = 0.d0
    do 50 i = 1, ldv
        vtest = vtest +zr(vectt+i-1)*zr(xsol+i-1)
        if (ddlsta(i) .eq. 0 .and. proj .eq. 1) then
            if (zr(vectt+i-1) .lt. zero) then
                call assert(.false.)
            endif
        endif
50  end do
!
    write (ifm,*) 'VAL1_STAB : ',vtest
!
    if (vtest .lt. 0.d0 .or. etat .eq. 0) then
        do 90 i = 1, ldv
            vectp(i) = zr(vectt+i-1)
90      continue
        csta = vtest
        goto 300
    endif
!
!     CALCUL DU CRITERE
!
    do 60 i = 1, ldh
        do 70 j = 1, ldh
            if (i .eq. j) then
                zr(b+(i-1)*ldh+j-1) = 1.d0
            else
                zr(b+(i-1)*ldh+j-1) = 0.d0
            endif
70      continue
60  end do
!
    cara='NFSP'
!
    call mgauss(cara, h, zr(b), ldh, ldh,&
                ldh, det, iret)
!
    proj = 0
!
    call mppsta(zr(b), ldh, v, ldv, ddlsta,&
                n, zr(vect2), ddlexc, indico, proj)
!
    err = dnrm2(ldv,zr(vect2),1)
    call dscal(ldv, one/err, zr(vect2), 1)
    call mrmult('ZERO', ldynfa, zr(vect2), zr(xsol), 1,&
                .true.)
    vtest = 0.d0
    do 55 i = 1, ldv
        vtest = vtest +zr(vect2+i-1)*zr(xsol+i-1)
55  end do
!
!      WRITE (IFM,*) 'VAL_PROPRE_MAX : ',VTEST
!
    do 15 i = 1, ldh
        do 25 j = 1, ldh
            zr(q+(i-1)*ldh+j-1) = -zr(b+(i-1)*ldh+j-1)- zr(b+(j-1)* ldh+i-1)
25      continue
15  end do
    do 35 i = 1, ldh
!        Q(I,I) = 2*VTEST*(1.D0/4.D0)+Q(I,I)
        zr(q+(i-1)*ldh+i-1) = 2*vtest*(1.5d0/2.d0)+ zr(q+(i-1)*ldh+i- 1)
35  end do
!
    if (redem .eq. 0) then
        do 80 i = 1, ldv
            vectp(i) = zr(vectt+i-1)
80      continue
    endif
!
    indico = 1
    proj = 1
!
    call mppsta(zr(q), ldh, v, ldv, ddlsta,&
                n, vectp, ddlexc, indico, proj)
!
    err = dnrm2(ldv,vectp,1)
    call dscal(ldv, one/err, vectp, 1)
    call mrmult('ZERO', ldynfa, vectp, zr(xsol), 1,&
                .true.)
    vtest = 0.d0
    do 65 i = 1, ldv
        vtest = vtest +vectp(i)*zr(xsol+i-1)
        if (ddlsta(i) .eq. 0) then
            if (vectp(i) .lt. zero) then
                call assert(.false.)
            endif
        endif
65  end do
!
    write (ifm,*) 'VAL2_STAB : ',vtest
    write (ifm,9070)
    write (ifm,9080)
!
    csta = vtest
!
300  continue
!
    redem = redem +1
!
    9070 format (72(' '))
    9080 format (72('-'))
!
! ----------------------------------------------
!
! --- MENAGE
    call jedetr('&&UNISTA.VECT.TEM1')
    call jedetr('&&UNISTA.VECT.TEM2')
    call jedetr('&&UNISTA.VECT.TEM3')
    call jedetr('&&UNISTA.VECT.TEM4')
    call jedetr('&&UNISTA.VECT.TEM5')
!
end subroutine

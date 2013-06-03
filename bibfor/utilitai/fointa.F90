subroutine fointa(ipif, nbpu, nompu, valpu, resu)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/fiintf.h'
    include 'asterc/r8prem.h'
    include 'asterc/r8vide.h'
    include 'asterfort/focoli.h'
    include 'asterfort/fointn.h'
    include 'asterfort/folocx.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: ipif, nbpu
    real(kind=8) :: valpu(*), resu
    character(len=*) :: nompu(*)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     INTERPOLATION POUR CALCULER RESU = F(X,Y,Z,...)
! ----------------------------------------------------------------------
! IN  IPIF  : POINTEUR DANS LE MATERIAU CODE (FONCTION OU NAPPE)
! IN  NBPU  : NOMBRE DE PARAMETRES DANS NOMPU ET VALPU
! IN  NOMPU : NOMS DES PARAMETRES "UTILISATEUR"
! IN  VALPU : VALEURS DES PARAMETRES "UTILISATEUR"
! OUT RESU  : R : RESULTAT DE L'INTERPOLATION
! ----------------------------------------------------------------------
!
!
!
    integer :: npar(2), indfct, jpro, jpar, lpara, nbvn, nbpara, i
    integer :: nupar, nbpt, jval, inume, ier, iret
    real(kind=8) :: tab(4), rpar, rvar, epsi, valr(4)
    real(kind=8) :: linlin, linlog, loglog, loglin, x, x1, y1, x2, y2
    character(len=1) :: coli
    character(len=19) :: nomf
    character(len=24) :: nompf(2)
!     ------------------------------------------------------------------
    integer :: iadzi, iazk24
    character(len=24) :: valk(3)
! ----------------------------------------------------------------------
! PARAMETER ASSOCIE AU MATERIAU CODE
!
    parameter  ( indfct = 7 )
! ----------------------------------------------------------------------
!     FONCTION EN LIGNE
!
    linlin(x,x1,y1,x2,y2)= y1+(x-x1)*(y2-y1)/(x2-x1)
    linlog(x,x1,y1,x2,y2)=exp(log(y1)+(x-x1)*(log(y2)-log(y1))&
     &                                        /(x2-x1))
    loglog(x,x1,y1,x2,y2)=exp(log(y1)+(log(x)-log(x1))*(log(y2)&
     &                                     -log(y1))/(log(x2)-log(x1)))
    loglin(x,x1,y1,x2,y2)=y1+(log(x)-log(x1))*(y2-y1)&
     &                                         /(log(x2)-log(x1))
!     ------------------------------------------------------------------
    npar(1) = 0
    npar(2) = 0
    nomf = ' '
    jpro = zi(ipif+1)
    jpar = zi(ipif+2)
    resu = r8vide()
    ier=0
!
    epsi = sqrt ( r8prem() )
!
! --- FONCTION "CONSTANT"
!
    if (zk24(jpro) .eq. 'CONSTANT') then
!         ------------------------
        resu = zr(jpar+1)
        goto 999
!
!
! --- FONCTION "INTERPRE" : FORMULE
!
    else if (zk24(jpro).eq.'INTERPRE') then
!             ------------------------
        nomf = zk24(jpro+5)(1:19)
        call fiintf(nomf, nbpu, nompu, valpu, iret,&
                    'E', resu)
        if (iret .ne. 0) then
            call tecael(iadzi, iazk24)
            call u2mesk('F+', 'FONCT0_9', 1, nomf)
            call u2mesk('F', 'FONCT0_10', 1, zk24(iazk24-1+3))
        endif
        goto 999
!
! --- AUTRES TYPES DE FONCTION
!
    else if (zk24(jpro).eq.'FONCTION') then
        nbpara = 1
        nompf(1) = zk24(jpro+2)
        nomf = zk24(jpro+5)(1:19)
!
    else if (zk24(jpro).eq.'NAPPE') then
        nbpara = 2
        nompf(1) = zk24(jpro+2)
        nompf(2) = zk24(jpro+6)
        nomf = zk24(jpro+5)(1:19)
!
    else
        call u2mesk('F', 'CALCULEL6_61', 1, zk24(jpro))
    endif
!
    do 20 i = 1, nbpara
        do 21 nupar = 1, nbpu
            if (nompu(nupar) .eq. nompf(i)) then
!           -- SI UN PARAMETRE EST FOURNI PLUSIEURS FOIS
!              ON PREND LE DERNIER (VOIR RCVALB)
                npar(i)=nupar
            endif
21      continue
        if (npar(i) .eq. 0) then
            valk(1)=nomf
            valk(2)=nompf(i)
            call tecael(iadzi, iazk24)
            valk(3) = zk24(iazk24-1+3)
            call u2mesk('F', 'CALCULEL6_62', 3, valk)
        endif
20  end do
!
! =====================================================================
!                          F O N C T I O N
! =====================================================================
!
    if (zk24(jpro) .eq. 'FONCTION') then
        nbpt = zi(ipif)
        jval = jpar + nbpt
        rvar = valpu(npar(1))
        call folocx(zr(jpar), nbpt, rvar, zk24(jpro+4), zi(ipif+ indfct),&
                    epsi, coli, ier)
        if (ier .ne. 0) goto 999
        call focoli(zi(ipif+indfct), coli, zk24(jpro+1), zr(jpar), zr(jval),&
                    rvar, resu, ier)
        if (ier .ne. 0) goto 999
!
! =====================================================================
!                            N A P P E
! =====================================================================
!
    else if (zk24(jpro) .eq. 'NAPPE   ') then
        rpar = valpu(npar(1))
        rvar = valpu(npar(2))
        lpara = zi(ipif+4)
        nbvn = zi(ipif+5)
        call folocx(zr(lpara), nbvn, rpar, zk24(jpro+4), zi(ipif+ indfct),&
                    epsi, coli, ier)
        if (ier .ne. 0) goto 999
        inume = zi(ipif+indfct)
!
        if (coli .eq. 'C') then
            call fointn(ipif, nomf, rvar, inume, epsi,&
                        resu, ier)
            if (ier .ne. 0) goto 999
!
        else if (coli.eq.'I') then
            if (zk24(jpro+1)(1:3) .eq. 'NON') then
                call u2mess('F', 'UTILITAI2_12')
            endif
            call fointn(ipif, nomf, rvar, inume, epsi,&
                        tab(3), ier)
            if (ier .ne. 0) goto 999
            call fointn(ipif, nomf, rvar, inume+1, epsi,&
                        tab(4), ier)
            if (ier .ne. 0) goto 999
!
! ------- INTERPOLATION FINALE SUR LES PARAMETRES
!
            tab(1) = zr(lpara+inume-1)
            tab(2) = zr(lpara+inume )
            if (zk24(jpro+1) .eq. 'LIN LIN ') then
                resu = linlin(rpar,tab(1),tab(3),tab(2),tab(4))
            else if (zk24(jpro+1).eq.'LIN LOG ') then
                resu = linlog(rpar,tab(1),tab(3),tab(2),tab(4))
            else if (zk24(jpro+1).eq.'LOG LOG ') then
                resu = loglog(rpar,tab(1),tab(3),tab(2),tab(4))
            else if (zk24(jpro+1).eq.'LOG LIN ') then
                resu = loglin(rpar,tab(1),tab(3),tab(2),tab(4))
            endif
!
        else if (coli.eq.'E') then
            call fointn(ipif, nomf, rvar, inume, epsi,&
                        tab(3), ier)
            if (ier .ne. 0) goto 999
            call fointn(ipif, nomf, rvar, inume+1, epsi,&
                        tab(4), ier)
            if (ier .ne. 0) goto 999
            tab(1) = zr(lpara+inume-1)
            tab(2) = zr(lpara+inume )
            resu = linlin(rpar,tab(1),tab(3),tab(2),tab(4))
!
        else
            call u2mesk('F', 'UTILITAI2_13', 1, coli)
        endif
!
    else
        call u2mesk('F', 'UTILITAI2_14', 1, zk24(jpro))
    endif
!
999  continue
    if (ier .ne. 0) then
        call u2mesg('F', 'CALCULEL6_63', 1, nomf, 1,&
                    ier, 0, valr)
    endif
!
!
end subroutine

subroutine optimw(method, nrupt, x, y, prob,&
                  sigw, nt, nur, nbres, calm,&
                  cals, mk, sk, mkp, skp,&
                  impr, ifm, dept, indtp, nbtp)
    implicit none
    include 'asterfort/ntweib.h'
    include 'asterfort/u2mess.h'
    integer :: nrupt, nt(*), nbres, nur(*), ir, indtp(*), nbtp, ifm
    real(kind=8) :: x(*), y(*), sigw(*), mk, sk(*), mkp, skp(*), prob(*)
    character(len=16) :: method
    logical :: calm, cals, impr, dept
!     ----------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ----------------------------------------------------------------
!     AUTEUR : M. BONNAMY
!     ----------------------------------------------------------------
!
!     BUT: CALCUL DE RECALAGE DES PARAMETRES DE WEIBULL
!
!     ----------------------------------------------------------------
!
!     METHOD       /IN/:METHODE DE CALAGE
!     NRUPT        /IN/:NOMBRE TOTAL DE CONTRAINTES DE WEIBULL
!     SIGW         /IN/:CONTRAINTES DE WEIBULL AUX INSTANTS DE RUPTURE
!     NT           /IN/:DIMENSION DE LA SOUS-BASE CORRESPONDANT A LA
!                       TEMPERATURE T
!     NUR          /IN/:NUMERO DE RESULTAT ASSOCIEE A
!                       LA CONTRAINTE SIGW(I)
!     NBRES        /IN/:NOMBRE DE BASES DE RESULTATS
!     MK           /IN/:PARAMETRE M(K)DE WEIBULL
!     SK           /IN/:PARAMETRE SIGMA-U(K) DE WEIBULL
!     CALM         /IN/:TRUE SI M EST FIXE
!     CALS         /IN/:TRUE SI SIGMA_U EST FIXE
!     IMPR         /IN/:IMPRESSION DETAILLEE
!     DEPT         /IN/:DEPENDANCE EN TEMPERATURE POUR SIGMA-U
!     INDTP        /IN/:INDICE DE TEMPERATURE POUR CHAQUE RESULTAT
!     NBTP         /IN/:NOMBRE DE TEMPERATURE DIFFERENTES
!
!     X,Y          /OUT/:VALEUR DES FONCTIONS LOG(SIGMAW)
!                        ET LOG(LOG(1/(1-PF)))
!     PROB         /OUT/:PROBABILITE THEORIQUE POUR REGRESSION LINEAIRE
!     MKP          /OUT/:PARAMETRE M(K+1)DE WEIBULL
!     SKP          /OUT/:PARAMETRE SIGMA-U(K+1) DE WEIBULL
!
!     ----------------------------------------------------------------
!
    real(kind=8) :: syi, sxi, sxixi, sxiyi, sxiyj, sxixj, unsurn, unsurm
    real(kind=8) :: swm, prec, mg, md, prov, snt, s1, s2
    integer :: i, j, k, itp, irg
!     ----------------------------------------------------------------
!
    if (method(1:9) .eq. 'REGR_LINE') then
!
!        METHODE DE REGRESSION LINEAIRE
!
!
        if (.not.dept) then
!
!       UN SEUL RESU : PAS DE DEPENDANCE EN TEMPERATURE
!
            syi = 0.d0
            sxi = 0.d0
            sxixi = 0.d0
            sxiyi = 0.d0
!
            do 10 i = 1, nrupt
!
                prob(i) = i
                s1 = nrupt
                prob(i) = prob(i) / (s1 + 1.d0)
                y(i) = log ( log ( 1.d0 / ( 1.d0-prob(i)) ) )
                x(i) = log ( sigw(i) )
                syi = syi + y(i)
                sxi = sxi + x(i)
                sxixi = sxixi + x(i)*x(i)
                sxiyi = sxiyi + x(i)*y(i)
!
10          continue
!
            sxiyj = 0.d0
            sxixj = 0.d0
!
            do 20 i = 1, nrupt
!
                do 30 j = 1, nrupt
!
                    sxiyj = sxiyj + x(i)*y(j)
                    sxixj = sxixj + x(i)*x(j)
!
30              continue
!
20          continue
!
            unsurn = nrupt
            unsurn = 1.d0/unsurn
!
            if ((.not.calm) .and. (.not.cals)) then
                mkp = (unsurn*sxiyj-sxiyi) / ( unsurn*sxixj-sxixi )
                skp(1) = exp ( unsurn*(sxi- (1.d0/mkp)*syi) )
            else if (calm) then
                mkp = mk
                skp(1) = exp ( unsurn*(sxi- (1.d0/mkp)*syi) )
            else if (cals) then
                skp(1) = sk(1)
                mkp = sxiyi/(sxixi - log(skp(1))*sxi)
            endif
            if (impr) write(ifm,*) 'M(K) =',mkp,'SIGU(K) = ',skp(1)
!
            sxi =0.d0
            do 40 j = 1, nrupt
!
                prov = (1.d0-exp(-(sigw(j)/sk(1))**mk))
                if (prov .ne. 1.d0) prov = log ( log (1.d0/(1.d0-prov) ) )
                sxi = sxi + (y(j)- prov)**2
!
40          continue
            if (impr) write(ifm, * ) 'ECART THEORIE-EXPERIENCE AU DEBUT DE L''ITERATION : ', sxi
!
        else
!
!       DEPENDANCE EN TEMPERATURE SIGMA_U(T)
!
            sxixi = 0.d0
            sxiyi = 0.d0
!
            do 110 itp = 1, nbtp
!
                snt = 0.d0
!
                do 120 ir = 1, nbres
!
                    if (indtp(ir) .eq. itp) snt = snt + nt(ir)
!
120              continue
!
                do 130 i = 1, nrupt
!
                    irg = 1
                    do 140 k = 1, i-1
                        if (indtp(nur(k)) .eq. itp) then
                            irg = irg+1
                        endif
140                  continue
!
                    if (indtp(nur(i)) .eq. itp) then
!
                        prob(i) = irg
                        prob(i) = prob(i) / (snt+1.d0)
                        y(i) = log ( log ( 1.d0 / ( 1.d0-prob(i)) ) )
                        x(i) = log ( sigw(i) )
                        sxixi = sxixi + x(i)*x(i)
                        sxiyi = sxiyi + x(i)*y(i)
!
                    endif
!
130              continue
!
110          continue
!
            s1 = 0.d0
            s2 = 0.d0
!
            do 210 itp = 1, nbtp
!
                sxiyj = 0.d0
                sxixj = 0.d0
                snt = 0.d0
!
                do 200 ir = 1, nbres
!
                    if (indtp(ir) .eq. itp) snt = snt + nt(ir)
!
200              continue
!
                do 300 i = 1, nrupt
!
                    do 400 j = 1, nrupt
!
                        if (indtp(nur(i)) .eq. itp .and. indtp(nur(j)) .eq. itp) then
                            sxiyj = sxiyj + x(i)*y(j)
                            sxixj = sxixj + x(i)*x(j)
                        endif
!
400                  continue
!
300              continue
                s1 = s1 + sxiyj/snt
                s2 = s2 + sxixj/snt
!
210          continue
!
            if ((.not.calm)) then
                mkp = (s1-sxiyi) / ( s2-sxixi )
            else if (calm) then
                mkp = mk
            endif
!
            if (impr) write(ifm,*) 'M(K) =',mkp
!
            if (((.not.calm).and.(.not.cals)) .or. calm) then
!
!          (M ET SIGMA-U) OU (SIGMA-U) SONT A RECALER
!
                do 211 itp = 1, nbtp
!
                    snt = 0.d0
!
                    do 101 ir = 1, nbres
!
                        if (indtp(ir) .eq. itp) snt = snt + nt(ir)
!
101                  continue
!
                    sxi = 0.d0
                    syi = 0.d0
!
                    do 201 i = 1, nrupt
!
                        if (indtp(nur(i)) .eq. itp) then
                            syi = syi + y(i)
                            sxi = sxi + x(i)
                        endif
!
201                  continue
!
                    skp(itp) = exp ( (sxi-(1.d0/mkp)*syi)/snt )
                    if (impr) write(ifm,*) 'S(K) (',itp,')=',skp(itp)
!
211              continue
!
            else if (cals) then
!
                do 301 ir = 1, nbtp
!
                    skp(ir) = sk(ir)
                    if (impr) write(ifm,*) 'S(K) (',ir,')=',skp(ir)
!
301              continue
!
            endif
!
        endif
!
    else if (method(1:9).eq.'MAXI_VRAI') then
!
!        METHODE DU MAXIMUM DE VRAISSEMBLANCE
!
        if ((.not.calm) .and. (.not.cals)) then
!
!        M ET SIGMA-U SONT A RECALER
!
            prec = 1.d-8
            mg = 1.d0
            md = mk
            swm = 0.d0
            unsurn = nrupt
            unsurn = 1.d0/unsurn
            if (impr) write(ifm,*) 'RESOLUTION F(M)=0 PAR NEWTON'
!
!          RESOLUTION DE L'EQUATION NON LINEAIRE F(M)=0
!
            call ntweib(nrupt, cals, sk, sigw, nur,&
                        nt, nbres, mg, md, prec,&
                        mkp, impr, ifm, indtp, nbtp)
!
            unsurm = 1.d0/mkp
!
!          CALCUL DU SIGMA-U
!
            do 12 itp = 1, nbtp
!
                snt = 0.d0
                do 11 ir = 1, nbres
!
                    if (indtp(ir) .eq. itp) snt = snt + nt(ir)
!
11              continue
!
                swm = 0.d0
                do 31 i = 1, nrupt
!
                    if (indtp(nur(i)) .eq. itp) then
                        swm = swm + sigw(i) ** mkp
                    endif
!
31              continue
!
                skp(itp) = ( swm / snt ) ** ( unsurm )
!
12          continue
!
        else if (calm) then
!
!        M EST CALE
!
            mkp = mk
            unsurm = 1.d0/mkp
!
            do 52 itp = 1, nbtp
!
                snt = 0.d0
                do 51 ir = 1, nbres
                    if (indtp(ir) .eq. itp) snt = snt + nt(ir)
51              continue
!
                swm = 0.d0
                do 41 i = 1, nrupt
!
                    if (indtp(nur(i)) .eq. itp) then
                        swm = swm + sigw(i) ** mkp
                    endif
!
41              continue
!
                skp(itp) = ( swm / snt ) ** ( unsurm )
!
52          continue
!
        else if (cals) then
!
!        SIGMA-U EST CALE
!
            do 71 ir = 1, nbtp
                skp(ir) = sk(ir)
71          continue
            prec = 1.d-8
            mg = 1.d0
            md = mk
!
!          RESOLUTION DE L'EQUATION NON LINEAIRE F(M)=0
!
            if (impr) write(ifm,*) 'RESOLUTION F(M)=0 PAR NEWTON'
            call ntweib(nrupt, cals, sk, sigw, nur,&
                        nt, nbres, mg, md, prec,&
                        mkp, impr, ifm, indtp, nbtp)
!
        endif
!
        if (impr) then
            write(ifm,*) 'M(K) =',mkp
            do 61 ir = 1, nbtp
                write(ifm,*) 'S(K) (',ir,')=',skp(ir)
61          continue
        endif
!
    endif
!
    if (mkp .lt. 1.d0) then
        call u2mess('F', 'UTILITAI3_36')
    endif
!
end subroutine

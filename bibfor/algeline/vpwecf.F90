subroutine vpwecf(option, typres, nfreq, mxfreq, resufi,&
                  resufr, resufk, lamor, ktyp, lns)
!-----------------------------------------------------------------------
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
!     ECRITURE DES FREQUENCES RELATIVEMENT A LA METHODE UTILISEE
!     IMPRESSION D'OFFICE SUR "MESSAGE"
!-----------------------------------------------------------------------
    implicit   none
!
! PARAMETRES D'APPEL
    include 'asterc/isnnem.h'
    include 'asterc/r8vide.h'
    include 'asterfort/assert.h'
    include 'asterfort/infniv.h'
    integer :: nfreq, mxfreq, resufi(mxfreq, *), lamor
    real(kind=8) :: resufr(mxfreq, *)
    character(len=*) :: option, resufk(mxfreq, *), typres
    character(len=1) :: ktyp
    logical :: lns
!
! VARIABLES LOCALES
    integer :: ifm, ifreq, ireso, iterb, iterj, iterq, itera, indf, niv
    real(kind=8) :: fff, am, err, prec, undf, cha, am2, erc, errmoy
    character(len=27) :: straux
!     ------------------------------------------------------------------
    call infniv(ifm, niv)
    undf = r8vide()
    indf = isnnem()
    cha = undf
    errmoy = 0.d0
    if (nfreq .eq. 0) call assert(.false.)
    if (resufk(nfreq,2) .eq. 'BATHE_WILSON') then
        if (typres .eq. 'DYNAMIQUE') then
            write(ifm,1000)
        else
            write(ifm,1001)
        endif
        do 10 ifreq = 1, nfreq
            ireso = resufi(ifreq,1)
            fff = resufr(ifreq,1)
            cha = resufr(ifreq,2)
            am = resufr(ifreq,4)
            iterb = resufi(ifreq,3)
            iterj = resufi(ifreq,5)
            errmoy = errmoy + abs(am)
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,1010) ireso,fff,am,iterb,iterj
            else
                write(ifm,1010) ireso,cha,am,iterb,iterj
            endif
10      continue
        write(ifm,7776)errmoy/nfreq
        write(ifm,7777)
!
    else if (resufk(nfreq,2) .eq. 'LANCZOS') then
        if (lamor .eq. 0) then
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,2000)
            else
                write(ifm,2001)
            endif
        else
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,2100)
            else
                write(ifm,2101)
            endif
        endif
        do 20 ifreq = 1, nfreq
            ireso = resufi(ifreq,1)
            fff = resufr(ifreq,1)
            cha = resufr(ifreq,2)
            if (lamor .eq. 0) then
                am = resufr(ifreq,4)
            else
                am = resufr(ifreq,3)
            endif
            iterq = resufi(ifreq,2)
            errmoy = errmoy + abs(am)
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,2010) ireso,fff,am,iterq
            else
                write(ifm,2010) ireso,cha,am,iterq
            endif
20      continue
        if (lamor .eq. 0) write(ifm,7776)errmoy/nfreq
        write(ifm,7777)
!
    else if (resufk(nfreq,2) .eq. 'SORENSEN') then
        if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,2200)
            else
                write(ifm,2201)
            endif
        else
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,2202)
            else
                write(ifm,2203)
            endif
        endif
        do 35 ifreq = 1, nfreq
            ireso = resufi(ifreq,1)
            fff = resufr(ifreq,1)
            cha = resufr(ifreq,2)
            if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
                am = resufr(ifreq,4)
                errmoy = errmoy + abs(am)
            else
                am = resufr(ifreq,3)
                erc = resufr(ifreq,4)
                errmoy = errmoy + abs(erc)
            endif
            if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
                if (typres .eq. 'DYNAMIQUE') then
                    write(ifm,2210) ireso,fff,am
                else
                    write(ifm,2210) ireso,cha,am
                endif
            else
                if (typres .eq. 'DYNAMIQUE') then
                    write(ifm,2211) ireso,fff,am,erc
                else
                    write(ifm,2211) ireso,cha,am,erc
                endif
            endif
35      continue
        write(ifm,7776)errmoy/nfreq
        write(ifm,7777)
!
    else if (resufk(nfreq,2)(1:2) .eq. 'QZ') then
        straux='ALGORITHME '//resufk(nfreq,2)(1:16)
        if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,3200)straux
            else
                write(ifm,3201)straux
            endif
        else
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,3202)straux
            else
                write(ifm,3203)straux
            endif
        endif
        do 36 ifreq = 1, nfreq
            ireso = resufi(ifreq,1)
            fff = resufr(ifreq,1)
            cha = resufr(ifreq,2)
            if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
                am = resufr(ifreq,4)
                errmoy = errmoy + abs(am)
            else
                am = resufr(ifreq,3)
                erc = resufr(ifreq,4)
                errmoy = errmoy + abs(erc)
            endif
            if ((lamor.eq.0) .and. (ktyp.eq.'R') .and. (.not.lns)) then
                if (typres .eq. 'DYNAMIQUE') then
                    write(ifm,3210) ireso,fff,am
                else
                    write(ifm,3210) ireso,cha,am
                endif
            else
                if (typres .eq. 'DYNAMIQUE') then
                    write(ifm,3211) ireso,fff,am,erc
                else
                    write(ifm,3211) ireso,cha,am,erc
                endif
            endif
36      continue
        write(ifm,7776)errmoy/nfreq
        write(ifm,7777)
!
        elseif ((resufk(nfreq,2) .eq. 'INVERSE_R' .or. resufk(nfreq,2)&
    .eq. 'INVERSE_C') .and. ( option(1:6) .eq. 'PROCHE') ) then
        if (typres .eq. 'DYNAMIQUE') then
            write(ifm,4000)
        else
            write(ifm,4001)
        endif
        do 40 ifreq = 1, nfreq
            ireso = resufi(ifreq,1)
            fff = resufr(ifreq,1)
            cha = resufr(ifreq,2)
            am = resufr(ifreq,3)
            iterq = resufi(ifreq,4)
            err = resufr(ifreq,15)
            am2 = resufr(ifreq,4)
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,4010) ireso,fff,am,iterq,err,am2
            else
                write(ifm,4010) ireso,cha,am,iterq,err,am2
            endif
            resufr(ifreq,14) = undf
            resufr(ifreq,15) = undf
            resufi(ifreq,2) = indf
            resufi(ifreq,3) = indf
            resufi(ifreq,4) = indf
            resufi(ifreq,8) = iterq
40      continue
        write(ifm,7777)
!
        elseif ( resufk(nfreq,2) .eq. 'INVERSE_R' .and. option(1:6) .eq.&
    'AJUSTE' ) then
        if (typres .eq. 'DYNAMIQUE') then
            write(ifm,5000)
        else
            write(ifm,5001)
        endif
        do 50 ifreq = 1, nfreq
            ireso = resufi(ifreq,1)
            fff = resufr(ifreq,1)
            cha = resufr(ifreq,2)
            am = resufr(ifreq,3)
            itera = resufi(ifreq,2)
            iterb = resufi(ifreq,3)
            prec = resufr(ifreq,14)
            iterq = resufi(ifreq,4)
            err = resufr(ifreq,15)
            am2 = resufr(ifreq,4)
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,5010) ireso,fff,am,itera,iterb,prec,iterq,&
                err,am2
            else
                write(ifm,5010) ireso,cha,am,itera,iterb,prec,iterq,&
                err,am2
            endif
            resufr(ifreq,14) = undf
            resufr(ifreq,15) = undf
            resufi(ifreq,2) = indf
            resufi(ifreq,3) = indf
            resufi(ifreq,4) = indf
            resufi(ifreq,7) = iterq
50      continue
        write(ifm,7777)
!
        elseif ( resufk(nfreq,2) .eq. 'INVERSE_R' .and. option(1:6) .eq.&
    'SEPARE' ) then
        if (typres .eq. 'DYNAMIQUE') then
            write(ifm,6000)
        else
            write(ifm,6001)
        endif
        do 60 ifreq = 1, nfreq
            ireso = resufi(ifreq,1)
            fff = resufr(ifreq,1)
            am = resufr(ifreq,3)
            cha = resufr(ifreq,2)
            itera = resufi(ifreq,2)
            iterq = resufi(ifreq,4)
            err = resufr(ifreq,15)
            am2 = resufr(ifreq,4)
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,6010) ireso,fff,am,itera,iterq,err,am2
            else
                write(ifm,6010) ireso,cha,am,itera,iterq,err,am2
            endif
            resufr(ifreq,14) = undf
            resufr(ifreq,15) = undf
            resufi(ifreq,2) = indf
            resufi(ifreq,3) = indf
            resufi(ifreq,4) = indf
            resufi(ifreq,6) = iterq
60      continue
        write(ifm,7777)
!
        elseif ( resufk(nfreq,2) .eq. 'INVERSE_C' .and. ( option(1:6)&
    .eq. 'AJUSTE' .or. option(1:6) .eq. 'SEPARE' ) ) then
        if (typres .eq. 'DYNAMIQUE') then
            write(ifm,7000)
        else
            write(ifm,7001)
        endif
        do 70 ifreq = 1, nfreq
            ireso = resufi(ifreq,1)
            fff = resufr(ifreq,1)
            am = resufr(ifreq,3)
            iterb = resufi(ifreq,2)
            prec = resufr(ifreq,14)
            iterq = resufi(ifreq,4)
            err = resufr(ifreq,15)
            am2 = resufr(ifreq,4)
            if (typres .eq. 'DYNAMIQUE') then
                write(ifm,7010) ireso,fff,am,iterb,prec,iterq,err,am2
            else
                write(ifm,7010) ireso,cha,am,iterb,prec,iterq,err,am2
            endif
            resufr(ifreq,14) = undf
            resufr(ifreq,15) = undf
            resufi(ifreq,2) = indf
            resufi(ifreq,3) = indf
            resufi(ifreq,4) = indf
            resufi(ifreq,8) = iterq
70      continue
        write(ifm,7777)
!
    endif
!
    1000 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION SIMULTANEE',/,&
     &        22x,'METHODE DE BATHE ET WILSON',/,/,&
     &       4x,'NUMERO',4x,'FREQUENCE (HZ)',4x,'NORME D''ERREUR',4x,&
     &            'ITER_BATHE',4x,'ITER_JACOBI' )
    1001 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION SIMULTANEE',/,&
     &        22x,'METHODE DE BATHE ET WILSON',/,/,&
     &      4x,'NUMERO',4x,'CHARGE CRITIQUE',4x,'NORME D''ERREUR',4x,&
     &            'ITER_BATHE',4x,'ITER_JACOBI' )
    1010 format (1p,6x,i4,5x,e12.5,6x,e12.5,7x,i4,10x,i4)
!
    2000 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION SIMULTANEE',/,&
     &        22x,'METHODE DE LANCZOS',/,/,&
     &       4x,'NUMERO',4x,'FREQUENCE (HZ)',4x,'NORME D''ERREUR',4x,&
     &            'ITER_QR' )
    2001 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION SIMULTANEE',/,&
     &        22x,'METHODE DE LANCZOS',/,/,&
     &       4x,'NUMERO',4x,'CHARGE CRITIQUE',4x,'NORME D''ERREUR',4x,&
     &            'ITER_QR' )
    2010 format (1p,6x,i4,5x,e12.5,6x,e12.5,7x,i4)
    2100 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION SIMULTANEE',/,&
     &        22x,'METHODE DE LANCZOS',/,/,&
     &        4x,'NUMERO',4x,'FREQUENCE (HZ)',4x,'AMORTISSEMENT',4x,&
     &            'ITER_QR' )
    2101 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION SIMULTANEE',/,&
     &        22x,'METHODE DE LANCZOS',/,/,&
     &        4x,'NUMERO',4x,'CHARGE CRITIQUE',4x,'AMORTISSEMENT',4x,&
     &            'ITER_QR' )
!
    2200 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION SIMULTANEE',/,&
     &        22x,'METHODE DE SORENSEN',/,/,&
     &        4x,'NUMERO',4x,'FREQUENCE (HZ)',4x,'NORME D''ERREUR')
    2201 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION SIMULTANEE',/,&
     &        22x,'METHODE DE SORENSEN',/,/,&
     &        4x,'NUMERO',4x,'CHARGE CRITIQUE',4x,'NORME D''ERREUR')
    2202 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION SIMULTANEE',/,&
     &        22x,'METHODE DE SORENSEN',/,/,&
     &        4x,'NUMERO',4x,'FREQUENCE (HZ)',4x,'AMORTISSEMENT',4x,&
     &        'NORME D''ERREUR')
    2203 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION SIMULTANEE',/,&
     &        22x,'METHODE DE SORENSEN',/,/,&
     &        4x,'NUMERO',4x,'CHARGE CRITIQUE',4x,'AMORTISSEMENT',4x,&
     &        'NORME D''ERREUR')
    2210 format (1p,6x,i4,5x,e12.5,6x,e12.5)
    2211 format (1p,6x,i4,5x,e12.5,6x,e12.5,6x,e12.5)
!
!
    3200 format ( 7x,'CALCUL MODAL:  METHODE GLOBALE DE TYPE QR',/,&
     &        22x,a27,/,/,&
     &        4x,'NUMERO',4x,'FREQUENCE (HZ)',4x,'NORME D''ERREUR')
    3201 format ( 7x,'CALCUL MODAL:  METHODE GLOBALE DE TYPE QR',/,&
     &        22x,a27,/,/,&
     &        4x,'NUMERO',4x,'CHARGE CRITIQUE',4x,'NORME D''ERREUR')
    3202 format ( 7x,'CALCUL MODAL:  METHODE GLOBALE DE TYPE QR',/,&
     &        22x,a27,/,/,&
     &        4x,'NUMERO',4x,'FREQUENCE (HZ)',4x,'AMORTISSEMENT',4x,&
     &        'NORME D''ERREUR')
    3203 format ( 7x,'CALCUL MODAL:  METHODE GLOBALE DE TYPE QR',/,&
     &        22x,a27,/,/,&
     &        4x,'NUMERO',4x,'CHARGE CRITIQUE',4x,'AMORTISSEMENT',4x,&
     &        'NORME D''ERREUR')
    3210 format (1p,6x,i4,5x,e12.5,6x,e12.5)
    3211 format (1p,6x,i4,5x,e12.5,6x,e12.5,6x,e12.5)
!
!
!
    4000 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION INVERSE',/,&
     &        54x,'INVERSE',/,&
     &        4x,'NUMERO',4x,'FREQUENCE (HZ)',4x,'AMORTISSEMENT',4x,&
     &        'NB_ITER',4x,'PRECISION',4x,'NORME D''ERREUR' )
    4001 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION INVERSE',/,&
     &        54x,'INVERSE',/,&
     &        4x,'NUMERO',4x,'CHARGE CRITIQUE',4x,'AMORTISSEMENT',4x,&
     &        'NB_ITER',4x,'PRECISION',4x,'NORME D''ERREUR' )
    4010 format (1p,6x,i4,5x,e12.5,6x,e12.5,5x,i4,4x,e12.5,6x,e12.5)
!
    5000 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION INVERSE',/,&
     &        48x,'DICHOTOMIE',7x,'SECANTE',17x,'INVERSE',/,&
     &        4x,'NUMERO',4x,'FREQUENCE (HZ)',4x,'AMORTISSEMENT',4x,&
     &        'NB_ITER',2(4x,'NB_ITER',4x,'PRECISION'),&
     &        4x,'NORME D''ERREUR')
    5001 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION INVERSE',/,&
     &        48x,'DICHOTOMIE',7x,'SECANTE',17x,'INVERSE',/,&
     &        4x,'NUMERO',4x,'CHARGE CRITIQUE',4x,'AMORTISSEMENT',4x,&
     &        'NB_ITER',2(4x,'NB_ITER',4x,'PRECISION'),&
     &        4x,'NORME D''ERREUR')
    5010 format (1p,6x,i4,5x,e12.5,6x,e12.5,5x,i4,7x,i4,4x,e12.5,4x,i4,&
     &        4x,e12.5,6x,e12.5)
!
    6000 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION INVERSE',/,&
     &        48x,'DICHOTOMIE',8x,'INVERSE',/,&
     &        4x,'NUMERO',4x,'FREQUENCE (HZ)',4x,'AMORTISSEMENT',4x,&
     &        'NB_ITER',4x,'NB_ITER',4x,'PRECISION',&
     &        4x,'NORME D''ERREUR')
    6001 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION INVERSE',/,&
     &        48x,'DICHOTOMIE',8x,'INVERSE',/,&
     &        4x,'NUMERO',4x,'CHARGE_CRITIQUE',4x,'AMORTISSEMENT',4x,&
     &        'NB_ITER',4x,'NB_ITER',4x,'PRECISION',&
     &        4x,'NORME D''ERREUR')
    6010 format (1p,6x,i4,5x,e12.5,6x,e12.5,5x,i4,7x,i4,4x,e12.5,&
     &        6x,e12.5)
!
    7000 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION INVERSE',/,&
     &        55x,'MULLER',17x,'INVERSE',/,&
     &        4x,'NUMERO',4x,'FREQUENCE (HZ)',4x,'AMORTISSEMENT',&
     &        2(4x,'NB_ITER',4x,'PRECISION'),4x,'NORME D''ERREUR')
    7001 format ( 7x,'CALCUL MODAL:  METHODE D''ITERATION INVERSE',/,&
     &        55x,'MULLER',17x,'INVERSE',/,&
     &        4x,'NUMERO',4x,'CHARGE_CRITIQUE',4x,'AMORTISSEMENT',&
     &        2(4x,'NB_ITER',4x,'PRECISION'),4x,'NORME D''ERREUR')
    7010 format (1p,6x,i4,5x,e12.5,6x,e12.5,5x,i4,4x,e12.5,4x,i4,4x,e12.5,&
     &        6x,e12.5)
!
    7776 format(' NORME D''ERREUR MOYENNE: ',e12.5)
    7777 format ( / )
!
!     ------------------------------------------------------------------
end subroutine

subroutine utroun(ch1, idec, ch2, isup)
    implicit   none
#include "asterfort/assert.h"
#include "asterfort/lxlgut.h"
#include "asterfort/lxliis.h"
    integer :: idec, isup
    character(len=24) :: ch1, ch2
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
! ----------------------------------------------------------------------
!
! BUT : TRONQUER UN REEL SOUS FORME EXPONENTIELLE A LA IDEC-EME DECIMALE
!
! EXEMPLE :
!      CH1 = 5.77350269789E+04
!
!      SI IDEC = 7  => CH2 = 5.7735027E+04
!      SI IDEC = 5  => CH2 = 5.77350E+04
!      SI IDEC = 3  => CH2 = 5.774E+04
!
! IN :
!    CH1   : CHAINE DE CARACATERES REPRESENTANT UNE VALEUR FLOTTANTE
!    IDEC  : POSITION DE LA DECIMALE OU L'ARRONDI VA S'OPERER
!    ISUP  : 0 : ON FAIT UN "ARRONDI" AU PLUS PRES.
!            1 : ON "ARRONDIT" SYSTEMATIQUEMENT AU DESSUS
!
! OUT :
!   CH2   : CHAINE DE CARACATERES REPRESENTANT LA VALEUR FLOTTANTE
!           ARRONDIE
! ----------------------------------------------------------------------
!
    integer :: lcv, ii, i, iii, itmp, iret, ic1
    character(len=1) :: next(10), c1, c2
    character(len=24) :: chv3, chv1, chv2
    logical(kind=1) :: lsg, lnext
    data next/'1','2','3','4','5','6','7','8','9','0'/
!
!
! --  INITIALISATIONS
!
    ASSERT(isup.eq.0.or.isup.eq.1)
    chv2 = ' '
    chv3 = ' '
    ch2 = ' '
    lnext= .false.
    if (ch1(1:1) .eq. '-') then
        lsg=.true.
        chv1=ch1(2:)
        ASSERT(isup.eq.0)
    else
        lsg=.false.
        chv1=ch1
    endif
!
!
! --- POSITION DE L'EXPOSANT DANS LA CHAINE : III
!
    lcv = lxlgut(chv1)
    ii=0
    do 10 i = 1, lcv
        if (chv1(i:i) .ne. 'E') then
            ii=ii+1
            goto 10
        endif
        goto 11
10  end do
11  continue
!
!     SI LA CHAINE NE CONTIENT PAS DE 'E' :
    if (ii .eq. lcv) ASSERT(.false.)
    iii=ii+1
!
!
! --- POSITION DU POINT DANS LA CHAINE : II
!
    ii=0
    do 20 i = 1, lcv
        if (chv1(i:i) .ne. '.') then
            ii=ii+1
            goto 20
        endif
        goto 21
20  end do
21  continue
!
!     SI LA CHAINE NE CONTIENT PAS DE POINT :
    if (ii .eq. lcv) ASSERT(.false.)
!
!     POSITION DANS LA CHAINE DU '.' : II
    ii=ii+1
!
!
! --- TRONCATURE :
!     ===========
!
!     ENTIER SITUE A LA IDEC-EME +1 DECIMALE : ITMP
!     SI ITMP> 5 : LA DECIMALE PRECEDENTE EST MODIFIEE
!     (ET EVENTUELLEMENT LES AUTRES DECIMALES ...)
!     SINON : ON PEUT TRONQUER SANS PROBLEME
!
!
    call lxliis(chv1(ii+idec+1:ii+idec+1), itmp, iret)
    ASSERT(iret.eq.0)
    ASSERT(itmp.ge.0.and.itmp.le.9)
!
!     -- POUR FORCER L'ARRONDI "AU DESSUS" :
    if (isup .eq. 1) itmp=9
!
    if (itmp .lt. 5) then
        chv2=chv1(1:ii+idec)//chv1(iii:iii+3)
    else
!
!          SOIT CHV3 LA FUTURE CHAINE CHV2 INVERSEE
!
!          1- ON COMMENCE PAR L'EXPOSANT
        do 30 i = 1, 4
            chv3(i:i)=chv1(iii+4-i:iii+4-i)
30      continue
!
!          2- ON TRAITE TOUTES LES DECIMALES
        lnext=.true.
        do 40 i = 1, idec
            c1=chv1(ii+idec-i+1:ii+idec-i+1)
            call lxliis(c1, ic1, iret)
            c2=next(ic1+1)
            if (lnext) then
                chv3(4+i:4+i)=c2
                if (c2 .eq. '0') then
                    lnext=.true.
                else
                    lnext=.false.
                endif
            else
                chv3(4+i:4+i)=c1
                lnext=.false.
            endif
40      continue
!
!          3- ON Y AJOUTE LE '.'
        chv3(4+idec+1:4+idec+1)='.'
!
!          4- ON TRAITE LA VALEUR ENTIERE
        do 50 i = 1, ii-1
            c1=chv1(ii-1-i+1:ii-1-i+1)
            call lxliis(c1, ic1, iret)
            c2=next(ic1+1)
            if (lnext) then
                chv3(4+idec+1+i:4+idec+1+i)=c2
                if (c2 .eq. '0') then
                    lnext=.true.
                else
                    lnext=.false.
                endif
            else
                chv3(4+idec+1+i:4+idec+1+i)=c1
                lnext=.false.
            endif
50      continue
!
!          5. ON PASSE A LA DIZAINE OU A LA CENTAINE SUPERIEURE SI LNEXT
        if (lnext) chv3(4+idec+1+ii:4+idec+1+ii)='1'
!
!          ON INVERSE CHV3 : CHV2
        lcv = lxlgut(chv3)
        do 60 i = 1, lcv
            chv2(i:i)=chv3(lcv-i+1:lcv-i+1)
60      continue
!
    endif
!
! --- FIN : ON RETOURNE CH2
!
    if (lsg) then
        ch2='-'//chv2
    else
        ch2=chv2
    endif
!
!
!
end subroutine

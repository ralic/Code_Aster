subroutine utfloa(floa, ch1, ch2)
    implicit   none
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/lxliis.h"
    logical(kind=1) :: floa
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
! BUT : ECRIRE UN REEL AVEC EXPOSANT EN UN REEL SANS EXPOSANT
!       OU ECRIT UN REEL AVEC EXPOSANT EN SUPPRIMANT LES ZEROS
!       INUTILES
!
! UTILISE POUR L'ECRITURE DANS TEST_RESU, TEST_TABLE, TEST_FONCTION :
!
! ARGUMENTS :
!
!   IN :
!     FLOA   : = TRUE  : SI ON DECIDE D'ECRIRE SANS EXPOSANT
!              = FALSE : SI ON CONSERVE L'ECRITURE AVEC EXPOSANT
!     CH1    : CHAINE DE CARACATERES REPRESENTANT UNE VALEUR REELLE
!              AVEC EXPOSANT
!
!   OUT :
!     CH2   : CHAINE DE CARACATERES REPRESENTANT LA VALEUR CH1
!             AVEC OU SANS EXPOSANT
!
!
! EXEMPLES :
!
!      - FLOA = TRUE :
!           a)  CH1 = 5.77352E+04   => CH2 = 57735.2
!           b)  CH1 = 1.547500E-02  => CH2 = 0.015475
!
!      - FLOA = FALSE :
!           a)  CH1 = 5.77352E+04   => CH2 = 5.77352E+04
!           b)  CH1 = 1.547500E-02  => CH2 = 1.5475E-02
!
!
! ----------------------------------------------------------------------
    integer :: i, iii, lcv, nexpo, iret, in, sign, ii, ndeci, n
    integer :: itmp
    character(len=2) :: chtmp
    character(len=24) :: chv3, chv1, chv2, chwk
!
    call jemarq()
!
!
! --- INITIALISATION
!
    chv1=' '
    chv2=' '
    chv3=' '
    chtmp=' '
    chwk=' '
    if (ch1(1:1) .eq. '-') then
        chv1=ch1(2:)
        ch2(1:1)='-'
    else
        chv1=ch1
        ch2(1:1)=' '
    endif
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
!     SI LA CHAINE NE CONTINENT PAS DE 'E' : ON S'ARRETE
    if (ii .eq. lcv) ASSERT(.false.)
    iii=ii+1
!
!     SIGNE : SIGN
    sign=0
    if (chv1(iii+1:iii+1) .eq. '-') then
        sign=-1
    else if (chv1(iii+1:iii+1).eq.'+') then
        sign=1
    endif
!
!     VALEUR DE L'EXPOSANT
    call lxliis(chv1(iii+2:iii+3), nexpo, iret)
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
!     SI LA CHAINE NE CONTINENT PAS DE POINT : ON S'ARRETE
    if (ii .eq. lcv) ASSERT(.false.)
!
!     POSITION DANS LA CHAINE DU '.' : II
    ii=ii+1
!
!
! --- REECRITURE (EVENTUELLE) DE CHV1 EN CHV3
!
    if (ii .eq. 2) then
        chv3=chv1
        in=sign*nexpo
    else
        chv3(1:1)=chv1(1:1)
        chv3(2:2)='.'
        do 30 i = 1, ii-2
            chv3(i+2:i+2)=chv1(i+1:i+1)
30      continue
        do 40 i = 1, iii-ii-1
            chv3(i+ii:i+ii)=chv1(i+ii:i+ii)
40      continue
        chv3(iii:iii)='E'
        in=ii-2+sign*nexpo
        if (in .ge. 0) then
            chv3(iii+1:iii+1)='+'
        else
            chv3(iii+1:iii+1)='-'
        endif
        call codent(abs(in), 'D0', chtmp)
        chv3=chv3(1:iii+1)//chtmp(1:2)
        nexpo=abs(in)
    endif
!
!     NOMBRE DE DECIMALES : NDECI
    ndeci=iii-3
!
!     ===================================================
!     ====  SI ON EFFECTUE L'ECRITURE SANS EXPOSANT  ====
!     ===================================================
!
    if (floa) then
!
!     --- EXPOSANT POSITIF ---
!
        if (in .ge. 0) then
!
            if (ndeci .gt. nexpo) then
                chv2(1:1)=chv3(1:1)
                do 50 i = 1, nexpo
                    chv2(1+i:1+i)=chv3(2+i:2+i)
50              continue
                chv2(2+nexpo:2+nexpo)='.'
!
                do 60 i = 1, ndeci-nexpo
                    chv2(i+2+nexpo:i+2+nexpo)=chv3(2+nexpo+i:2+nexpo+&
                    i)
60              continue
                n=lxlgut(chv2)
                itmp=0
                do 61 i = n, 1, -1
                    if (chv2(i:i) .eq. '.') then
                        goto 66
                    endif
                    if (chv2(i:i) .eq. '0') then
                        itmp=itmp+1
                    else
                        goto 66
                    endif
61              continue
66              continue
                if (itmp .gt. 0) then
                    chwk=chv2
                    chv2=' '
                    chv2=chwk(1:n-itmp)
                    if (chv2(n-itmp:n-itmp) .eq. '.') then
                        chv2(n-itmp+1:n-itmp+1)='0'
                    endif
                endif
!
            else
                chv2(1:1)=chv3(1:1)
                do 150 i = 1, ndeci
                    chv2(1+i:1+i)=chv3(2+i:2+i)
150              continue
                if (ndeci .eq. nexpo) then
                    chv2(2+ndeci:3+ndeci)='.0'
                else
                    do 151 i = 1, nexpo-ndeci
                        chv2(1+ndeci+i:1+ndeci+i)='0'
151                  continue
                    chv2(2+nexpo:3+nexpo)='.0'
                endif
            endif
!
!     --- EXPOSANT NEGATIF ---
!
        else if (in.lt.0) then
!
            chv2(1:2)='0.'
            do 70 i = 1, nexpo-1
                chv2(2+i:2+i)='0'
70          continue
            chv2(2+nexpo:2+nexpo)=chv3(1:1)
            do 80 i = 1, ndeci
                chv2(2+nexpo+i:2+nexpo+i)=chv3(2+i:2+i)
80          continue
!
        endif
!
!
!     ===================================================
!     ====  SI ON CONSERVE L'ECRITURE AVEC EXPOSANT  ====
!     ===================================================
!
    else
!
        n=lxlgut(chv3)
        itmp=0
        do 261 i = iii-1, 1, -1
            if (chv3(i:i) .eq. '.') then
                goto 266
            endif
            if (chv3(i:i) .eq. '0') then
                itmp=itmp+1
            else
                goto 266
            endif
261      continue
266      continue
        if (itmp .gt. 0) then
            chv2=chv3(1:iii-1-itmp)
            if (chv2(iii-1-itmp:iii-1-itmp) .eq. '.') then
                chv2(iii-itmp:iii-itmp)='0'
            endif
            n=lxlgut(chv2)
            chv2(n+1:n+4)=chv3(iii:iii+3)
        else
            chv2=chv3
        endif
!
!
!
!
    endif
!
!
!
!
! --- FIN : ON RETOURNE CHV2 EN PRENANT EN COMPTE LE SIGNE
!     ----------------------------------------------------
!
    if (ch2(1:1) .eq. '-') then
        ch2(2:)=chv2
    else
        ch2=chv2
    endif
!
    call jedema()
!
end subroutine

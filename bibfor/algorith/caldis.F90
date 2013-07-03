subroutine caldis(fremax, fremin, pas, frexci, nbptmd,&
                  nbmode, lismod, fremod, amomod, nindex,&
                  npdsc3, frefin)
    implicit none
#include "jeveux.h"
#include "asterfort/discrt.h"
#include "asterfort/disexc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ordr8.h"
#include "asterfort/wkvect.h"
    integer :: nbptmd, nbmode, nindex, npdsc3, lismod(*)
    real(kind=8) :: fremax, fremin, pas, fremod(*), amomod(*), frefin
    character(len=4) :: frexci
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
!
!  BUT: CALCUL DE LA DISCRETISATION FREQUENTIELLE POUR
!                 LE CALCUL DYNAMIQUE ALEATOIRE
!
! IN  : FREMAX : FREQ MAX DE LA DISCRETISATION
! IN  : FREMIN : FREQ MIN DE LA DISCRETISATION
! IN  : PAS    : PAS DE LA DISCRETISATION
! IN  : FREXCI : FREQUENCES EXCITATION: AVEC OU SANS
! IN  : NBPTMD : NOMBRE DE POINTS PAR PICS
! IN  : NBMODE : NOMBRE DE MODES DYNAMIQUES
! IN  : LISMOD : LISTE DES NUMEROS D'ORDRE DES MODES
! IN  : FREMOD : FREQUENCE DU MODE MECA
! IN  : AMOMOD : AMORTISSEMENTS MODAUX
! IN  : NINDEX : NOMBRE  D INDICES RECUPERES DANS L INTERPECTRE EXC
! OUT : NPDSC3 : NOMBRE DE VALEURS DE FREQUENCE
! OUT : FREFIN : DERNIERE FREQUENCE ( LA PLUS GRANDE)
!-----------------------------------------------------------------------
!
    real(kind=8) :: frema1, fredeb
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i1, i2, iadii1, iadr1, iadsc1, iadsc2, iadsc3
    integer :: ibid1, icont1, ifreq1, igex1, ilfex, illex, ilong1
    integer :: imode, inajou, nbmax, npdsc0, npdsc2, mxval
    real(kind=8) :: amor, f0, f1, f2, freq
    real(kind=8) :: pasmin, r8ecar
!-----------------------------------------------------------------------
    call jemarq()
!
!---------CAS D UNE DISCRETISATION DEMANDEE PAR L UTILISATEUR
!
    call jeveuo('&&OP0131.LIADRFEX1', 'E', ilfex)
    call jeveuo('&&OP0131.LIADRLEX1', 'E', illex)
!
    if ((fremin.ne.-1.d0) .and. (fremax.ne.-1.d0)) then
        if (pas .eq. -1.d0) then
            pas = ( fremax - fremin ) / 100.d0
        endif
        npdsc3 = int( (fremax-fremin) / pas +1.d-6) + 1
        call wkvect('&&OP0131.DISCR3', 'V V R8', npdsc3, iadsc3)
        do 331,i1=1,npdsc3-1
        zr(iadsc3-1+i1) = fremin + (i1-1)*pas
331      continue
        zr(iadsc3-1+npdsc3) = fremax
        fredeb = fremin
        frefin = fremax
        goto 332
    endif
!
!---------CALCUL DE LA DISCRETISATION MINI IMPOSEE PAR LES FREQUENCES
!         PROPRES
!
!---------ON ALLOUE UN TABLEAU DE TRAVAIL POUR STOCKER TOUTES LES
!         DISCRETISATIONS
!
    ilong1=0
    if (frexci .eq. 'AVEC') then
        do 329,igex1=1,nindex*(nindex+1)/2
        ilong1=ilong1+zi(illex)
329      continue
    endif
    call wkvect('&&OP0131.DISCR1', 'V V R8', nbmode*nbptmd+ilong1 +nbmode, iadsc1)
    frema1=0.d0
    do 301,i1=1,nbmode
    imode = lismod(i1)
    f0 = fremod(imode)
    if (f0 .gt. frema1) frema1 = f0
    amor = amomod(i1)
    fredeb = 0.d0
    frefin = 2.d0*f0
    call discrt(f0, fredeb, frefin, nbptmd, amor,&
                zr(iadsc1+ (i1-1)* nbptmd))
    301 end do
    icont1=0
    mxval = nindex*(nindex+1)/2
    if (frexci .eq. 'AVEC') then
        do 330,igex1=1,nindex*(nindex+1)/2
        iadr1=zi(illex+mxval+1)
        do 316,i2=1,zi(illex)
        icont1=icont1+1
        zr(iadsc1+nbmode*nbptmd-1+icont1)=zr(iadr1-1+i2)
316      continue
330      continue
    endif
!
!
!----AJOUT DES FREQUENCES PROPRES DANS LE CAS D AMORTISSEMENT NON NUL
!
    do 327,i2 = 1,nbmode
    imode = lismod(i2)
    amor = amomod(i2)
    freq = fremod(imode)
    if (amor .ne. 0.d0) then
        zr(iadsc1+nbmode*nbptmd-1+ilong1+i2) = freq
    endif
    327 end do
    npdsc0 = ilong1+nbmode
!
!---------TRI DES FREQUENCES OBTENUES-RANGEMENT DANS &&..DISCR2
!
    call wkvect('&&OP0131.DISCI1', 'V V I', nbmode*nbptmd+npdsc0, iadii1)
    call ordr8(zr(iadsc1), nbmode*nbptmd+npdsc0, zi(iadii1))
    call wkvect('&&OP0131.DISCR2', 'V V R8', nbmode*nbptmd+npdsc0+50, iadsc2)
    zr(iadsc2)=zr(iadsc1-1+zi(iadii1))
    i2=1
    do 302,i1=2,nbmode*nbptmd+npdsc0
    if (zr(iadsc1-1+zi(iadii1+i1-1)) .gt. zr(iadsc1-1 +zi(iadii1+i1- 2))) then
        i2=i2+1
        zr(iadsc2+i2-1)=zr(iadsc1-1+zi(iadii1+i1-1))
    endif
    302 end do
    npdsc2=i2
!
!---------PRISE EN COMPTE DU PAS MINI
!
!  - ON TIENT COMPTE D'UN PAS MINI EXPRIME OU NON PAR L UTILISATEUR
!  --- IL EST PAR DEFAUT EGAL A (FREFIN -FREDEB)/100
!
    if ((fremin.eq.-1.d0) .and. (fremax.eq.-1.d0)) then
        fredeb=0.d0
        frefin=2.0d0*frema1
    else
        fredeb=fremin
        frefin=fremax
    endif
    if (pas .eq. -1.d0) then
        pasmin=(frefin-fredeb)/100.0d0
    else
        pasmin=pas
    endif
    nbmax=int((frefin-fredeb)/pasmin)
    call wkvect('&&OP0131.DISCR3', 'V V R8', npdsc2+nbmax, iadsc3)
    npdsc3=npdsc2
    inajou=0
    zr(iadsc3)=zr(iadsc2)
    do 304,ifreq1=2,npdsc2
    f2=zr(iadsc2-1+ifreq1)
    f1=zr(iadsc2-1+ifreq1-1)
    r8ecar=f2-f1
    if ((r8ecar.gt.pasmin) .and. (f2.le.frefin)) then
        inajou=int(r8ecar/pasmin)
        do 305,ibid1=1,inajou
        zr(iadsc3-1+ifreq1-1+npdsc3-npdsc2+ibid1)= zr(iadsc2+&
                ifreq1-2)+ibid1*pasmin
305      continue
    endif
    npdsc3=npdsc3+inajou
    inajou=0
    zr(iadsc3-1+ifreq1+npdsc3-npdsc2)=zr(iadsc2-1+ifreq1)
    304 end do
332  continue
!
!------CALCUL DES DSP EXCITS DANS LA DISCRETISATION REPONSE
!
    call disexc(nindex, ilfex, illex, npdsc3, iadsc3)
    call jedema()
end subroutine

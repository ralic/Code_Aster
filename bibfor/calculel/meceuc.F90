subroutine meceuc(stop, poux, option, caraez, ligrel,&
                  nin, lchin, lpain, nou, lchou,&
                  lpaou, base)
!
! aslint: disable=W1306
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/assach.h"
#include "asterfort/assert.h"
#include "asterfort/barych.h"
#include "asterfort/calcul.h"
#include "asterfort/chlici.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/sepach.h"
    integer :: nin, nou
    character(len=1) :: stop
    character(len=8) :: poux, carael
    character(len=*) :: base, option
    character(len=*) :: lchin(*), lchou(*), lpain(*), lpaou(*), ligrel, caraez
!
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
!  BUT : CETTE ROUTINE EST UN INTERMEDIAIRE VERS LA ROUTINE CALCUL.F
!        POUR CERTAINES OPTIONS DONT LE RESULTAT EST COMPLEXE, ON
!        APPELLE 2 FOIS CALCUL EN AYANT SEPARE LES CHAMPS COMPLEXES"IN"
!        EN 2 : PARTIE REELLE ET PARTIE IMAGINAIRE
!
!
!     ENTREES:
!        STOP   :  /'S' : ON S'ARRETE SI AUCUN ELEMENT FINI DU LIGREL
!                         NE SAIT CALCULER L'OPTION.
!                  /'C' : ON CONTINUE SI AUCUN ELEMENT FINI DU LIGREL
!                         NE SAIT CALCULER L'OPTION. IL N'EXISTE PAS DE
!                         CHAMP "OUT" DANS CE CAS.
!        OPTIO  :  NOM D'1 OPTION
!        LIGREL :  NOM DU LIGREL SUR LEQUEL ON DOIT FAIRE LE CALCUL
!        NIN    :  NOMBRE DE CHAMPS PARAMETRES "IN"
!        NOU    :  NOMBRE DE CHAMPS PARAMETRES "OUT"
!        LCHIN  :  LISTE DES NOMS DES CHAMPS "IN"
!        LCHOU  :  LISTE DES NOMS DES CHAMPS "OUT"
!        LPAIN  :  LISTE DES NOMS DES PARAMETRES "IN"
!        LPAOU  :  LISTE DES NOMS DES PARAMETRES "OUT"
!        BASE   :  'G' , 'V' OU 'L'
!
!     SORTIES:
!       ALLOCATION ET CALCUL DES OBJETS CORRESPONDANT AUX CHAMPS "OUT"
!     CETTE ROUTINE MET EN FORME LES CHAMPS EVENTUELLEMENT COMPLEXE
!     POUR L APPEL A CALCUL
!-----------------------------------------------------------------------
!
!
    character(len=19) :: chdecr(nin), chdeci(nin), ch19, chr, chi, ch1, ch2
    character(len=19) :: lchinr(nin), lchini(nin)
    character(len=16) :: optio2
    character(len=8) :: nomgd
    integer :: ibid, k, iexi, iexi1, iexi2
    integer :: inddec(nin)
    logical :: lcmplx, lsspt, ldbg, lopdec
! ----------------------------------------------------------------------
!
    call jemarq()
!
    optio2=option
    carael=caraez
    ch1='&&MECEUC.CH1'
    ch2='&&MECEUC.CH2'
    ldbg=.true.
!
!
!     -- 0. LA ROUTINE N'EST UTILE QUE POUR CERTAINES OPTIONS :
!     ---------------------------------------------------------
    lopdec=.false.
    if (option .eq. 'ECIN_ELEM') lopdec=.true.
    if (option .eq. 'EFGE_ELGA') lopdec=.true.
    if (option .eq. 'EFGE_ELNO') lopdec=.true.
    if (option .eq. 'ENEL_ELGA') lopdec=.true.
    if (option .eq. 'ENEL_ELNO') lopdec=.true.
    if (option .eq. 'EPOT_ELEM') lopdec=.true.
    if (option .eq. 'EPSI_ELGA') lopdec=.true.
    if (option .eq. 'EPSI_ELNO') lopdec=.true.
    if (option .eq. 'SIEF_ELGA') lopdec=.true.
    if (option .eq. 'SIEF_ELNO') lopdec=.true.
    if (option .eq. 'SIGM_ELGA') lopdec=.true.
    if (option .eq. 'SIGM_ELNO') lopdec=.true.
    if (option .eq. 'SIPM_ELNO') lopdec=.true.
    if (option .eq. 'SIPO_ELNO') lopdec=.true.
    if (.not.lopdec) then
        call calcul(stop, optio2, ligrel, nin, lchin,&
                    lpain, nou, lchou, lpaou, base,&
                    'OUI')
        goto 9999
    endif
!
!
!     -- 1. Y-A-T-IL DES CHAMPS "IN" COMPLEXES ?
!           SI OUI, IL FAUT LES DECOUPER
!     -----------------------------------------------------------
    lcmplx=.false.
    do 1, k=1,nin
    inddec(k)=0
    if (lpain(k) .eq. ' ') goto 1
    ch19=lchin(k)
    if (ch19 .eq. ' ') goto 1
    if (ldbg) call chlici(ch19, 19)
    call exisd('CHAMP', ch19, iexi)
    if (iexi .eq. 0) goto 1
    call dismoi('F', 'NOM_GD', ch19, 'CHAMP', ibid,&
                nomgd, ibid)
!        -- MECHPO CREE PARFOIS UN CHAMP DE FORC_C
!           IL M'EMBETE ! COMMENT SAVOIR S'IL EST PERTINENT ?
    if (nomgd .eq. 'FORC_C') goto 1
!
    if (nomgd(5:6) .eq. '_C') then
        lcmplx=.true.
        inddec(k)=1
        chr='&&MECEUC.CHXX.R'
        chi='&&MECEUC.CHXX.I'
        call codent(k, 'D0', chr(12:13))
        call codent(k, 'D0', chi(12:13))
        call sepach(carael, ch19, 'V', chr, chi)
        chdecr(k)=chr
        chdeci(k)=chi
    endif
    1 end do
!
!
!     -- 2. S'IL N'Y A AUCUN CHAMP COMPLEXE, C'EST FACILE :
!     -------------------------------------------------------
    if (.not.lcmplx) then
        call calcul(stop, optio2, ligrel, nin, lchin,&
                    lpain, nou, lchou, lpaou, base,&
                    'OUI')
        goto 9999
    endif
!
!
!     -- 3. LE CHAMP "OUT" EST-IL A SOUS-POINTS ?
!     -------------------------------------------
    call assert(nou.eq.1)
    call exisd('CHAM_ELEM_S', lchou(1), iexi)
    lsspt=(iexi.ne.0)
!
!
!     -- 4.0 ON PREPARE LCHINR ET LCHINI :
!     ------------------------------------
    do 2,k=1,nin
    if (inddec(k) .eq. 0) then
        lchinr(k)=lchin(k)
        lchini(k)=lchin(k)
    else
        lchinr(k)=chdecr(k)
        lchini(k)=chdeci(k)
    endif
    2 end do
!
!
!     -- 4.1 APPEL A CALCUL AVEC LES PARTIES REELLES :
!     ------------------------------------------------
    if (lsspt) call copisd('CHAM_ELEM_S', 'V', lchou(1), ch1)
    call calcul(stop, optio2, ligrel, nin, lchinr,&
                lpain, nou, ch1, lpaou, 'V',&
                'OUI')
!
!
!     -- 4.2 APPEL A CALCUL AVEC LES PARTIES IMAGINAIRES :
!     ----------------------------------------------------
    if (lsspt) call copisd('CHAM_ELEM_S', 'V', lchou(1), ch2)
    call calcul(stop, optio2, ligrel, nin, lchini,&
                lpain, nou, ch2, lpaou, 'V',&
                'OUI')
!
!
!     -- 4.3 SI STOP='C' ET QUE CH1 ET CH2 N'EXISTENT PAS :
!     -----------------------------------------------------
    if (stop .eq. 'C') then
        call exisd('CHAM_ELEM', ch1, iexi1)
        call exisd('CHAM_ELEM', ch2, iexi2)
        if (iexi1 .eq. 0) then
            call assert(iexi2.eq.0)
            goto 9998
        endif
    endif
!
!
!     -- 6.  ASSEMBLAGE (R,I) OU CUMUL (R+I) :
!     -----------------------------------------
    if ((optio2.eq.'SIEF_ELNO') .or. (optio2.eq.'SIGM_ELGA') .or. (optio2.eq.'SIGM_ELNO')&
        .or. (optio2.eq.'EFGE_ELGA') .or. (optio2.eq.'EFGE_ELNO') .or.&
        (optio2.eq.'SIPM_ELNO') .or. (optio2.eq.'SIPO_ELNO') .or. (optio2.eq.'EPSI_ELNO')&
        .or. (optio2.eq.'EPSI_ELGA') .or. (optio2.eq.'STRX_ELGA') .or.&
        (optio2.eq.'SIEF_ELGA')) then
        call assach(ch1, ch2, base, lchou(1))
!
        elseif ((optio2.eq.'EPOT_ELEM') .or. (optio2.eq.'ENEL_ELGA') .or.&
    (optio2.eq.'ENEL_ELNO') .or. (optio2.eq.'ECIN_ELEM')) then
        call barych(ch1, ch2, 1.d0, 1.d0, lchou(1),&
                    'G')
    else
        call assert(.false.)
    endif
!
!
!
!     -- 7. MENAGE :
!     --------------
9998  continue
    do 3,k=1,nin
    if (inddec(k) .ne. 0) then
        call detrsd('CHAMP', chdecr(k))
        call detrsd('CHAMP', chdeci(k))
    endif
    3 end do
!
    call detrsd('CHAMP', ch1)
    call detrsd('CHAMP', ch2)
    call detrsd('CHAM_ELEM_S', ch1)
    call detrsd('CHAM_ELEM_S', ch2)
!
!
9999  continue
    call jedema()
end subroutine

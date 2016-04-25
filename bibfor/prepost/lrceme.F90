subroutine lrceme(chanom, nochmd, typech, nomamd, nomaas,&
                  nommod, nomgd, typent, nbcmpv, ncmpva,&
                  ncmpvm, prolz, iinst, numpt, numord,&
                  inst, crit, prec, nrofic, option,&
                  param, nbpgma, nbpgmm, nbspmm, codret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
!     LECTURE D'UN CHAMP AUX ELEMENTS - FORMAT MED
!     -    -       -         -               --
!-----------------------------------------------------------------------
!     ENTREES:
!        CHANOM : NOM ASTER DU CHAMP A LIRE
!        NOCHMD : NOM MED DU CHAMP DANS LE FICHIER
!        TYPECH : TYPE DE CHAMP AUX ELEMENTS : ELEM/ELGA/ELNO
!        TYPENT : TYPE D'ENTITE DU CHAMP
!                (MED_NOEUD,MED_MAILLE,MED_NOEUD_MAILLE)
!        NOMAMD : NOM MED DU MAILLAGE LIE AU CHAMP A LIRE
!                  SI ' ' : ON SUPPOSE QUE C'EST LE PREMIER MAILLAGE
!                           DU FICHIER
!        NOMAAS : NOM ASTER DU MAILLAGE
!        NOMMOD : NOM ASTER DU MODELE NECESSAIRE POUR LIGREL
!        NOMGD  : NOM DE LA GRANDEUR ASSOCIEE AU CHAMP
!        NBCMPV : NOMBRE DE COMPOSANTES VOULUES
!                 SI NUL, ON LIT LES COMPOSANTES A NOM IDENTIQUE
!        NCMPVA : LISTE DES COMPOSANTES VOULUES POUR ASTER
!        NCMPVM : LISTE DES COMPOSANTES VOULUES DANS MED
!        PROLZ  : VALEUR DE PROL_ZERO ('OUI' OU 'NAN')
!        IINST  : 1 SI LA DEMANDE EST FAITE SUR UN INSTANT, 0 SINON
!        NUMPT  : NUMERO DE PAS DE TEMPS EVENTUEL
!        NUMORD : NUMERO D'ORDRE EVENTUEL DU CHAMP
!        INST   : INSTANT EVENTUEL
!        CRIT   : CRITERE SUR LA RECHERCHE DU BON INSTANT
!        PREC   : PRECISION SUR LA RECHERCHE DU BON INSTANT
!        NROFIC : NUMERO NROFIC LOGIQUE DU FICHIER MED
!     SORTIES:
!        CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_____________________________________________________________________
!
! aslint: disable=W1504
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/as_mfdfdi.h"
#include "asterfort/as_mfdnfc.h"
#include "asterfort/as_mfdnfd.h"
#include "asterfort/as_mficlo.h"
#include "asterfort/as_mfiope.h"
#include "asterfort/cescar.h"
#include "asterfort/cescel.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvis.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/lrcame.h"
#include "asterfort/ulisog.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: chanom
    character(len=*) :: ncmpva, ncmpvm
    character(len=8) :: nommod, nomaas, nomgd
    character(len=4) :: typech
    character(len=3) :: prolz
    character(len=8) :: crit, param
    character(len=24) :: option
    character(len=*) :: nochmd, nomamd
!
    integer :: nrofic, typent
    integer :: nbcmpv
    integer :: iinst, numpt, numord
    integer :: nbpgma(*), nbpgmm(*), nbspmm(*)
    integer :: codret
!
    real(kind=8) :: inst
    real(kind=8) :: prec
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRCEME' )
!
    integer :: iaux, naux, unite, nbcham, nbcmp, jcmp
    integer :: tycha, junit
    integer :: vali(2)
    integer :: ibid
    integer :: jcesl
    integer :: ifm, nivinf, idfimd, nseqca
    integer :: jnocmp, ncmprf, jcmpva, nbcmpa, iret, i, j, nncp
    integer :: edlect
    parameter (edlect=0)
!
    character(len=1) :: saux01
    character(len=8) :: saux08
    character(len=19) :: chames, ligrel
    character(len=64) :: nomcha
    character(len=64) :: valk(1)
    character(len=200) :: nofimd
    character(len=255) :: kfic
!
    aster_logical :: ttt
!
    call jemarq()
!
    call infniv(ifm, nivinf)
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'DEBUT DE '//nompro
    endif
    1001 format(/,10('='),a,10('='),/)
!
!====
! 1. ALLOCATION D'UN CHAM_ELEM_S  (CHAMES)
!====
!
! 1.1. ==> REPERAGE DES CARACTERISTIQUES DE CETTE GRANDEUR
!
    call jenonu(jexnom ( '&CATA.GD.NOMGD', nomgd ), iaux)
    if (iaux .eq. 0) then
        call utmess('F', 'MED_65')
    endif
    call jeveuo(jexnom ( '&CATA.GD.NOMCMP', nomgd ), 'L', jnocmp)
    call jelira(jexnom ( '&CATA.GD.NOMCMP', nomgd ), 'LONMAX', ncmprf)
!
! 1.2. ==> ALLOCATION DU CHAM_ELEM_S
!
!               1234567890123456789
    chames = '&&      .CES.MED   '
    chames(3:8) = nompro
    ligrel = nommod//'.MODELE'
    if (nommod .eq. ' ') ligrel = ' '
!
    call jeexin(ncmpva, iret)
    if (iret .gt. 0) then
        call jeveuo(ncmpva, 'L', jcmpva)
        call jelira(ncmpva, 'LONMAX', nbcmpa)
        if (nomgd(1:4) .eq. 'VARI') then
            jnocmp=jcmpva
            ncmprf=nbcmpa
        else if (nbcmpa.le.ncmprf) then
            do 20 i = 1, nbcmpa
                ttt=.false.
                do 30 j = 1, ncmprf
                    if (zk8(jcmpva+i-1) .eq. zk8(jnocmp+j-1)) then
                        ttt=.true.
                    endif
 30             continue
                if (.not.ttt) then
                    call utmess('F', 'MED_66')
                endif
 20         continue
        else
            call utmess('F', 'MED_70')
        endif
!
    else
!
        call getvis(' ', 'UNITE', scal=unite, nbret=iaux)
        call ulisog(unite, kfic, saux01)
        if (kfic(1:1) .eq. ' ') then
            call codent(unite, 'G', saux08)
            nofimd = 'fort.'//saux08
        else
            nofimd = kfic(1:200)
        endif
        call as_mfiope(idfimd, nofimd, edlect, iret)
        call as_mfdnfd(idfimd, nbcham, iret)
        do 777 i = 1, nbcham
            call as_mfdnfc(idfimd, i, nbcmp, iret)
            call wkvect('&&LRCEME.NOMCMP_K16', 'V V K16', nbcmp, jcmp)
            call wkvect('&&LRCEME.UNITCMP', 'V V K16', nbcmp, junit)
            call as_mfdfdi(idfimd, i, nomcha, tycha, zk16(jcmp),&
                           zk16(junit), nseqca, iret)
            if (nomcha .eq. nochmd) then
                ncmprf=nbcmp
                call wkvect('&&LRCEME.NOMCMP_K8', 'V V K8', nbcmp, jnocmp)
                do 778 j = 1, nbcmp
                    zk8(jnocmp+j-1)=zk16(jcmp+j-1)(1:8)
778             continue
                call jedetr('&&LRCEME.NOMCMP_K16')
                call jedetr('&&LRCEME.UNITCMP')
                goto 780
            endif
            call jedetr('&&LRCEME.NOMCMP_K16')
            call jedetr('&&LRCEME.UNITCMP')
777     continue
        call as_mficlo(idfimd, iret)
    endif
!
780 continue
!
!====
! 3. LECTURE POUR CHAQUE TYPE DE SUPPORT
!====
!
    call lrcame(nrofic, nochmd, nomamd, nomaas, ligrel,&
                option, param, typech, typent, nbpgma,&
                nbpgmm, nbspmm, nbcmpv, ncmpva, ncmpvm,&
                iinst, numpt, numord, inst, crit,&
                prec, nomgd, ncmprf, jnocmp, chames,&
                codret)
!
    call jeveuo(chames//'.CESL', 'E', jcesl)
!
!====
! 4. TRANSFORMATION DU CHAM_ELEM_S EN CHAM_ELEM :
!====
!
!
    if (typech(1:4) .eq. 'CART') then
        call cescar(chames, chanom, 'V')
        nncp=0
    else
        call cescel(chames, ligrel, option, param, prolz,&
                    nncp, 'V', chanom, 'F', ibid)
    endif
    if (nncp .gt. 0) then
        iaux=0
        call jelira(chames//'.CESL', 'LONMAX', naux)
        do 40 i = 1, naux
            if (zl(jcesl+i-1)) iaux=iaux+1
 40     continue
        vali (1) = iaux
        vali (2) = nncp
        valk (1) = nochmd
        call utmess('A', 'MED_83', nk=1, valk=valk, ni=2, vali=vali)
    endif
!
    call detrsd('CHAM_ELEM_S', chames)
!
!====
! 5. BILAN
!====
!
    if (codret .ne. 0) then
        call utmess('A', 'MED_55', sk=chanom)
    endif
!
!      IF(TYPECH(1:4).EQ.'ELGA')THEN
!        CALL JEDETR('&&LRCEME_NBPG_MAILLE')
!      ENDIF
    call jedetr('&&LRCEME.NOMCMP_K8')
    call jedema()
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
!
end subroutine

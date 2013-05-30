subroutine corich(action, champ, ichin, ichout)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/juveca.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: action, champ
    integer :: ichin, ichout
! ----------------------------------------------------------------------
!  BUT :   GERER UN EVENTUEL LIEN ENTRE UN CHAMP (RESUELEM OU CHAM_NO)
!          ET LE NUMERO DE LA CHARGE AUQUEL IL EST ASSOCIE POUR POUVOIR
!          LUI APPLIQUER LE BON FONC_MULT.
!   CE LIEN EST UTILISE PAR LA ROUTINE ASASVE (RESUELEM)
!   CE LIEN EST UTILISE PAR LA ROUTINE ASCOVA (CHAM_NO)
!
!
! IN  ACTION  K1 : / 'E' : ON ECRIT UN LIEN
!                  / 'L' : ON LIT UN LIEN ECRIT AU PREALABLE
!                  / 'S' : ON SUPPRIME UN LIEN ECRIT AU PREALABLE
! IN          CHAMP   K19: NOM DU CHAMP
! IN     ICHIN     I : NUMERO DE LA CHARGE ASSOCIEE A CHAMP
! OUT    ICHOUT    I : NUMERO DE LA CHARGE ASSOCIEE A CHAMP
! ----------------------------------------------------------------------
! LE LIEN ENTRE CHAMP ET ICHIN EST CONSERVE DANS 2 OBJETS VOLATILS
! DE NOM : '&&CORICH.'//NOMU//'.REPT'
!          '&&CORICH.'//NOMU//'.NUCH'
! OU NOMU EST LE NOM DU CONCEPT UTILISATEUR RESULTAT DE LA COMMANDE
!
! CES OBJETS SONT DONC PROPRES A UNE COMMANDE (NOMU) UTILISANT
! LE MOT CLE EXCIT/FONC_MULT
!
!  CONVENTIONS :
! --------------
! SI ACTION='E', ICHOUT EST INUTILISE  (IBID)
!   SI ICHIN > 0 :ICHIN EST BIEN LA NUMERO DE LA CHARGE ASSOCIEE A CHAMP
!                 SI IL EXISTE UNE FONC_MULT, ELLE LUI SERA APPLIQUEE.
!   SI ICHIN = -1 :CHAMP N'EST ASSOCIE A AUCUNE FONC_MULT
!                 (CHARGEMENT DE DILATATION PAR EXEMPLE)
!   SI ICHIN = -2 :CHAMP EST "BIDON" : NUL OU INEXISTANT
!   SI ICHIN = 0 OU ICHIN < -2  : ERREUR FATALE.
!
! SI ACTION='L', ICHIN EST INUTILISE  (IBID)
!    SI ON A FAIT AU PREALABLE UN LIEN AVEC CHAMP, ON REND ICHOUT
!    SINON ON REND ICHOUT=0
!    ICHOUT=0  VEUT DIRE QUE LE CHAMP N'EST PAS RENSEIGNE :
!         SOIT IL NE L'A JAMAIS ETE, SOIT IL A ETE EFFACE (ACTION:'S')
!
! SI ACTION='S', ICHIN ET ICHOUT SONT INUTILISES  (IBID)
!    SI ON A FAIT AU PREALABLE UN LIEN AVEC CHAMP : OK
!    SINON  : ERREUR FATALE.
!
! ----------------------------------------------------------------------
!
!
!
!
    integer :: iret, long, longmx, ibid, k, kk, jnumic
    character(len=8) :: nomu
    character(len=16) :: nomcmd, kbid
    character(len=24) :: repert, numich, nomch, reptmp, nomch2
!
    call jemarq()
!
    call getres(nomu, kbid, nomcmd)
    repert = '&&CORICH.'//nomu//'.REPT'
    numich = '&&CORICH.'//nomu//'.NUCH'
    reptmp = '&&CORICH.REPTMP'
!
!
!     -- ALLOCATION DE REPERT ET NUMICH :
!     ------------------------------------------------------
    call jeexin(repert, iret)
    if (iret .eq. 0) then
        call jecreo(repert, 'V N K24')
        call jeecra(repert, 'NOMMAX', 50, kbid)
        call wkvect(numich, 'V V I', 50, ibid)
    endif
!
!
!     -- AGGRANDISSEMENT DE REPERT ET NUMICH SI NECESSAIRE:
!     ------------------------------------------------------
    call jelira(repert, 'NOMMAX', longmx, kbid)
    call jelira(repert, 'NOMUTI', long, kbid)
    if (long .gt. longmx-1) then
!
        call juveca(numich, 2*longmx)
!
        call jedupo(repert, 'V', reptmp, .false.)
        call jedetr(repert)
        call jecreo(repert, 'V N K24')
        call jeecra(repert, 'NOMMAX', 2*longmx, kbid)
        do 10,k = 1,long
        call jenuno(jexnum(reptmp, k), nomch)
        call jecroc(jexnom(repert, nomch))
10      continue
        call jedetr(reptmp)
!
    endif
!
!
!     -- CAS ACTION = 'E'
!     ------------------------------------------------------
    if (action .eq. 'E') then
        if (ichin .eq. 0) call u2mess('F', 'ASSEMBLA_13')
        if (ichin .lt. -2) call u2mess('F', 'ASSEMBLA_14')
        nomch = champ(1:19)
        call jenonu(jexnom(repert, nomch), kk)
        if (kk .eq. 0) call jecroc(jexnom(repert, nomch))
        call jenonu(jexnom(repert, nomch), kk)
        call jeveuo(numich, 'E', jnumic)
        zi(jnumic-1+kk) = ichin
!
!
!     -- CAS ACTION = 'L'
!     ------------------------------------------------------
    else if (action.eq.'L') then
        nomch = champ(1:19)
        call jenonu(jexnom(repert, nomch), kk)
        if (kk .eq. 0) then
            ichout = 0
        else
            call jeveuo(numich, 'L', jnumic)
            ichout = zi(jnumic-1+kk)
        endif
!
!
!     -- CAS ACTION = 'S'
!     ------------------------------------------------------
    else if (action.eq.'S') then
        nomch = champ(1:19)
        call jenonu(jexnom(repert, nomch), kk)
        if (kk .eq. 0) then
            call u2mess('F', 'ASSEMBLA_15')
        else
            call jeveuo(numich, 'E', jnumic)
            zi(jnumic-1+kk) = 0
        endif
!
!
!     -- CAS ACTION IMPREVUE :
!     ------------------------------------------------------
    else
        call u2mess('F', 'ASSEMBLA_16')
    endif
!
!
!     IMPRESSIONS POUR LE DEBUG :
!     ----------------------------
    if (.false.) then
        write (6,*) 'CORICH FIN ',action,' ',champ,ichin,ichout
        call jelira(repert, 'NOMUTI', long, kbid)
        call jelira(repert, 'NOMMAX', longmx, kbid)
        call jeveuo(numich, 'L', jnumic)
        write (6,*) 'CORICH FIN ETAT DU REP.: ',longmx,' ',long
        do 20,k = 1,long
        call jenuno(jexnum(repert, k), nomch2)
        write (6,*) 'CORICH FIN ETAT DU REP.: ',k,' ',nomch2,' ',&
            zi(jnumic-1+k)
20      continue
    endif
!
    call jedema()
end subroutine

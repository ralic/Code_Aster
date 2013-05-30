subroutine ircnme(ifi, nochmd, chanom, typech, modele,&
                  nbcmp, nomcmp, partie, numpt, instan,&
                  numord, nbnoec, linoec, sdcarm, codret)
!_______________________________________________________________________
! person_in_charge: nicolas.sellenet at edf.fr
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
!        IMPRESSION DU CHAMP CHANOM NOEUD ENTIER/REEL
!        AU FORMAT MED
!     ENTREES:
!       IFI    : UNITE LOGIQUE D'IMPRESSION DU CHAMP
!       NOCHMD : NOM MED DU CHAM A ECRIRE
!       PARTIE: IMPRESSION DE LA PARTIE IMAGINAIRE OU REELLE POUR
!               UN CHAMP COMPLEXE
!       CHANOM : NOM ASTER DU CHAM A ECRIRE
!       TYPECH : TYPE DU CHAMP
!       MODELE : MODELE ASSOCIE AU CHAMP
!       NBCMP  : NOMBRE DE COMPOSANTES A ECRIRE
!       NOMCMP : NOMS DES COMPOSANTES A ECRIRE
!       NUMPT  : NUMERO DE PAS DE TEMPS
!       INSTAN : VALEUR DE L'INSTANT A ARCHIVER
!       NUMORD : NUMERO D'ORDRE DU CHAMP
!       NBNOEC : NOMBRE DE NOEUDS A ECRIRE (O, SI TOUS LES NOEUDS)
!       LINOEC : LISTE DES NOEUDS A ECRIRE SI EXTRAIT
!       SDCARM : CARA_ELEM (UTILE POUR LES SOUS-POINTS)
!    SORTIES:
!       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_______________________________________________________________________
!
    implicit none
    include 'jeveux.h'
!
! 0.1. ==> ARGUMENTS
!
    include 'asterfort/cnocns.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/ircame.h'
    include 'asterfort/irmopr.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesk.h'
    character(len=8) :: typech, modele, sdcarm
    character(len=19) :: chanom
    character(len=64) :: nochmd
    character(len=*) :: nomcmp(*), partie
!
    integer :: nbcmp, numpt, ifi, numord
    integer :: nbnoec
    integer :: linoec(*)
!
    real(kind=8) :: instan
!
    integer :: codret
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRCNME' )
!
    character(len=19) :: chamns, nonvpr
    parameter ( nonvpr = '&&IRCNME.NV_PRO' )
!
    integer :: jcnsk, jcnsd, jcnsc, jcnsv, jcnsl, jnvpro, nbnvpr
!     ------------------------------------------------------------------
!
    call jemarq()
!
!====
! 1. PREALABLE
!====
!
!    --- CONVERSION CHAM_NO -> CHAM_NO_S
!               1234567890123456789
    chamns = '&&      .CNS.MED   '
    chamns(3:8) = nompro
    call cnocns(chanom, 'V', chamns)
!
!     MODIFICATION DU PROFIL
    call irmopr(chamns, nbnoec, linoec, nonvpr)
    call jeveuo(nonvpr, 'L', jnvpro)
    nbnvpr = zi(jnvpro)
!
!    --- ON RECUPERE LES OBJETS
!
    call jeveuo(chamns//'.CNSK', 'L', jcnsk)
    call jeveuo(chamns//'.CNSD', 'L', jcnsd)
    call jeveuo(chamns//'.CNSC', 'L', jcnsc)
    call jeveuo(chamns//'.CNSV', 'L', jcnsv)
    call jeveuo(chamns//'.CNSL', 'L', jcnsl)
!
!====
! 2. ECRITURE DES CHAMPS AU FORMAT MED
!====
!
    call ircame(ifi, nochmd, chanom, typech, modele,&
                nbcmp, nomcmp, ' ', partie, numpt,&
                instan, numord, jcnsk, jcnsd, jcnsc,&
                jcnsv, jcnsl, nbnvpr, zi(jnvpro+1), sdcarm,&
                codret)
!
!====
! 3. ON NETTOIE
!====
!
    call detrsd('CHAM_NO_S', chamns)
    call jedetr(nonvpr)
!
!====
! 4. BILAN
!====
!
    if (codret .ne. 0 .and. codret .ne. 100) then
        call u2mesk('A', 'MED_89', 1, chanom)
    endif
!
    call jedema()
!
end subroutine

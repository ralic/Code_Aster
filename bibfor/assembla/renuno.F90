subroutine renuno(nu, renum)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/reexi1.h'
    include 'asterfort/relagm.h'
    include 'asterfort/rercmk.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    character(len=14) :: nu
    character(len=*) :: renum
!     -------------------------------------------------
!     BUT:  CETTE ROUTINE A 3 FONCTIONS DISTINCTES:
!
!     1) RENUMEROTER (EVENTUELLEMENT) LES NOEUDS (DU MAILLAGE)
!     D'UN MODELE (ARGUMENT RENUM)
!     POUR L'INSTANT LA SEULE METHODE DISPONIBLE EST CELLE DE :
!     "REVERSE CUTHIL MAC-KEE" (CF. LE LIVRE :
!     "MATRIC COMPUTATION FOR ENGINEERS AND SCIENTISTS"
!     DE ALAN JENNINGS (WILEY ANS SONS))
!
!     2) RENUMEROTER  LES NOEUDS TARDIFS DU MAILLAGE
!     (NOEUDS DE LAGRANGE PROVENANT DES SOUS_STRUCTURES)
!     (CES NOEUDS DOIVENT EN EFFET TOUJOURS ENCADRER LES
!     NOEUDS PHYSIQUES CONTRAINTS)
!
!     3) RENDRE A NUME_DDL L'OBJET .EXI1
!     (POUR DIRE QUELS SONT LES NOEUDS (TARDIFS OU NON)
!     INTERVENANT REELLEMENT DANS LA NUMEROTATION)
!
!     IN:
!     ---
!     RENUM: 'RCMK' :  REVERSE-CUTHIL-MAC-KEE.
!            ' '    :  PAS DE RENUMEROTATION.
!     NU : NOM DU NUME_DDL  AUQUEL ON VA AJOUTER  LES OBJETS
!     .NEWN ET .OLDN
!     (ON SE SERT EN ENTREE DU SEUL OBJET NU//'.NUME.LILI')
!
!     OUT:
!     ---- NU EST COMPLETE PAR .NEWN , .OLDN ET .EXI1 (BASE : VOLATILE)
!
!     SOIT NM LE NOMBRE DE NOEUDS PHYSIQUES DU MAILLAGE (LILI(1))
!     NL LE NOMBRE DE NOEUDS TARDIFS DU MAILLAGE
!     N2 LE NOMBRE DE NOEUDS TARDIFS DU MODELE (LILI(2))
!     N3 LE NOMBRE DE NOEUDS TARDIFS DE LA 1ERE CHARGE(LILI(3))
!     .....
!     NP LE NOMBRE DE NOEUDS TARDIFS DE LA DERE CHARGE(LILI(P))
!
!     NBNOM = NM+NL      (NOMBRE MAX DE NOEUDS DU MAILLAGE)
!     NBNOT = N2+...+NP  (NOMBRE MAX DE NOEUDS TARDIFS DU MODELE
!     ET DE LA LISTE DE CHARGES)
!     NBNTT = NBNOM+NBNOT
!
!     .NEWN(*) EST DIMENSIONNE A NBNOM
!     .OLDN(*) EST DIMENSIONNE A NBNOM
!
!     .EXI1(*) EST DIMENSIONNE A NBNTT+1
!
!     SOIT LA NUMEROTATION IMPLICITE TOTALE :
!     1- LES NOEUDS PHYSIQUES DU MAILLAGE (NI) / ORDRE DE .NOMNOE
!     2- LES NOEUDS TARDIFS DU MAILLAGE (&I)   /
!     3- LES NOEUDS TARDIFS DU MODELE   (&LMI)
!     4- LES NOEUDS TARDIFS DE LA CHARGE 1 (&LCH1I)
!     - ...
!     - LES NOEUDS TARDIFS DE LA CHARGE P (&LCH1P)
!
!     LA RENUMEROTATION ('RCMK',..) NE CONCERNE EN FAIT QUE LES NOEUDS
!     DU MAILLAGE (NM+NL) CAR LES AUTRES NOEUDS DEVRONT DE TOUTES
!     FACONS ETRE RENUMEROTES PAR NUEFFE (LAGRANGES)
!
!     POUR I=1,NBNOM
!     .NEWN(I) EST LA POSITION DU NOEUD I DANS LA NOUVELLE NUMEROTATI
!     (SI NEWN(I) = 0 CE NOEUD N'EXISTE PAS DANS NUME_DDL)
!
!     .OLDN(I) EST LE NUMERO (ANCIEN) DU NOEUD NUMEROTE I DANS LA
!     NOUVELLE NUMEROTATION.
!
!
!     POUR I=1,NBNTT
!     .EXI1(1) =1 (CORRESPOND AU NOEUD "ZERO" FICTIF DE NUEFFE)
!     .EXI1(I+1) >0 SI LE NOEUD I EXISTE DANS LE NUME_DDL
!     =0 SINON
!
!     -----------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    integer :: nm, nl, nbnom, nbntt
    character(len=8) :: ma, mo, kbid
!
!
!     -- CALCUL DE .EXI1 :
!     --------------------
!-----------------------------------------------------------------------
    integer :: i, iaexi1, ianewn, iaoldn, ico, nlili
!-----------------------------------------------------------------------
    call jemarq()
    call reexi1(nu, mo, ma, nlili, nm,&
                nl, nbntt)
    nbnom = nm + nl
!
!     -- ALLOCATION DES OBJETS .NEWN ET .OLDN :
!     -----------------------------------------
    call wkvect(nu//'.NEWN', 'V V I', nbnom, ianewn)
    call wkvect(nu//'.OLDN', 'V V I', nbnom, iaoldn)
!
!
!     1ERE ETAPE: RENUMEROTATION EVENTUELLE DES NOEUDS PHYSIQUES:
!     (A LA FIN DE CETTE ETAPE, NEWN ET OLDN CONCERNENT UNIQUEMENT
!     LES NOEUDS PHYSIQUES (1->NM))
!     --------------------------------------------------------------
    call jeveuo(nu//'.EXI1', 'L', iaexi1)
!
    if (renum(1:4) .eq. 'RCMK') then
!     -- 'REVERSE-CUTHIL-MAC-KEE':
        call rercmk(nu, mo, ma, nlili, nm,&
                    nl, nbntt)
        else if (renum(1:4).eq.'SANS' .or. renum(1:2).eq.'MD' .or.&
    renum(1:5).eq.'METIS' .or. renum(1:3).eq.'AMD' .or. renum(1:3)&
    .eq.'AMF' .or. renum(1:4).eq.'QAMD' .or. renum(1:4).eq.'PORD'&
    .or. renum(1:6).eq.'SCOTCH'.or. renum(1:4).eq.'AUTO' )then
!     -- 'SANS RENUMEROTATION CUTHIL-MAC-KEE':
        ico = 0
        do 10,i = 1,nm
        if (zi(iaexi1+i) .gt. 0) then
            ico = ico + 1
            zi(ianewn-1+i) = ico
            zi(iaoldn-1+ico) = i
        endif
10      continue
    else
        call u2mesk('F', 'ASSEMBLA_37', 1, renum(1:4))
    endif
!
!
!     2EME ETAPE:
!     -- ON REMET LES NOEUDS TARDIFS DU MAILLAGE AU BON ENDROIT:
!     (A LA FIN DE CETTE ETAPE, NEWN ET OLDN CONCERNENT TOUS LES
!     NOEUDS DU MAILLAGE (1->NM+NL))
!     --------------------------------------------------------------
    call relagm(mo, ma, nm, nl, zi(ianewn),&
                zi(iaoldn))
!
!
!     -- ON MET A JOUR 'LONUTI' DE .OLDN : NOMBRE DE NOEUDS DU MAILLAGE
!     (RENUMEROTES OU NON) PARTICIPANT AU NUME_DDL:
!
    do 20,i = 1,nbnom
    if (zi(iaoldn-1+i) .eq. 0) then
        call jeecra(nu//'.OLDN', 'LONUTI', i-1, kbid)
        goto 30
    endif
    20 end do
!
!
30  continue
    call jedema()
end subroutine

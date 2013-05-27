subroutine irmmma(fid, nomamd, nbmail, connex, point,&
                  typma, nommai, prefix, nbtyp, typgeo,&
                  nomtyp, nnotyp, renumd, nmatyp, infmed,&
                  modnum, nuanom)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     ECRITURE DU MAILLAGE -  FORMAT MED - LES MAILLES
!        -  -     -                  -         --
!-----------------------------------------------------------------------
!     ENTREE:
!       FID    : IDENTIFIANT DU FICHIER MED
!       NOMAMD : NOM DU MAILLAGE MED
!       NBMAIL : NOMBRE DE MAILLES DU MAILLAGE
!       CONNEX : CONNECTIVITES
!       POINT  : VECTEUR POINTEUR DES CONNECTIVITES (LONGUEURS CUMULEES)
!       TYPMA  : VECTEUR TYPES DES MAILLES
!       NOMMAI : VECTEUR NOMS DES MAILLES
!       PREFIX : PREFIXE POUR LES TABLEAUX DES RENUMEROTATIONS
!                A UTILISER PLUS TARD
!       NBTYP  : NOMBRE DE TYPES POSSIBLES POUR MED
!       TYPGEO : TYPE MED POUR CHAQUE MAILLE
!       NNOTYP : NOMBRE DE NOEUDS POUR CHAQUE TYPE DE MAILLES
!       NOMTYP : NOM DES TYPES POUR CHAQUE MAILLE
!       RENUMD : RENUMEROTATION DES TYPES ENTRE MED ET ASTER
!       INFMED : NIVEAU DES INFORMATIONS A IMPRIMER
!       MODNUM : INDICATEUR SI LA SPECIFICATION DE NUMEROTATION DES
!                NOEUDS DES MAILLES EST DIFFERENTES ENTRE ASTER ET MED:
!                     MODNUM = 0 : NUMEROTATION IDENTIQUE
!                     MODNUM = 1 : NUMEROTATION DIFFERENTE
!       NUANOM : TABLEAU DE CORRESPONDANCE DES NOEUDS MED/ASTER.
!                NUANOM(ITYP,J): NUMERO DANS ASTER DU J IEME NOEUD DE LA
!                MAILLE DE TYPE ITYP DANS MED.
!
!     SORTIE:
!       NMATYP : NOMBRE DE MAILLES PAR TYPE
!-----------------------------------------------------------------------
!
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/mfcone.h'
    include 'asterfort/mfnome.h'
    include 'asterfort/mfnume.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: ntymax
    parameter (ntymax = 69)
!
! 0.1. ==> ARGUMENTS
!
    integer :: fid
    integer :: nbmail, nbtyp
    integer :: connex(*), typma(*), point(*)
    integer :: typgeo(*), nnotyp(*), nmatyp(*)
    integer :: renumd(*), modnum(ntymax), nuanom(ntymax, *)
    integer :: infmed
!
    character(len=6) :: prefix
    character(len=8) :: nommai(*)
    character(len=8) :: nomtyp(*)
    character(len=*) :: nomamd
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRMMMA' )
!
    integer :: edfuin
    parameter (edfuin=0)
    integer :: edmail
    parameter (edmail=0)
    integer :: ednoda
    parameter (ednoda=0)
!
    integer :: codret
    integer :: ipoin, ityp, letype
    integer :: ino
    integer :: ima, ipe18, ipe15
    integer :: jnomma(ntymax), jnumma(ntymax), jcnxma(ntymax)
    integer :: ifm, nivinf
!
    character(len=8) :: saux08
!
    logical :: lnocen
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
    call infniv(ifm, nivinf)
!
!====
! 2. PREPARATION DES TABLEAUX PAR TYPE DE MAILLE
!====
!
! 2.1. ==> DECOMPTE DU NOMBRE DE MAILLES PAR TYPE
!          EN FAIT, ON VEUT JUSTE SAVOIR S'IL Y EN A OU PAS.
!
    do 211 , ityp = 1 , ntymax
    nmatyp(ityp) = 0
    211 end do
!
    do 212 , ima = 1, nbmail
    nmatyp(typma(ima)) = nmatyp(typma(ima)) + 1
    212 end do
!
!     ON TRAITE LES PENTA18 EN OUBLIANT LES NOEUDS DU
!     CENTRE ET LES SEG4 EN OUBLIANT LES 2 NOEUDS CENTRAUX
    lnocen=.false.
!     - PENTA18
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA18'), ipe18)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA15'), ipe15)
    if (nmatyp(ipe18) .ne. 0) then
        nmatyp(ipe15)=nmatyp(ipe15)+nmatyp(ipe18)
        nmatyp(ipe18)=0
        lnocen=.true.
    endif
    if (lnocen) then
        call u2mess('A', 'PREPOST_86')
    endif
!
! 2.2. ==> ON VERIFIE QUE L'ON SAIT ECRIRE LES MAILLES PRESENTES DANS
!          LE MAILLAGE
!
    do 22 , ityp = 1, ntymax
!
    if (nmatyp(ityp) .ne. 0) then
        if (typgeo(ityp) .eq. 0) then
            call u2mesk('F', 'PREPOST2_93', 1, nomtyp(ityp))
        endif
    endif
!
    22 end do
!
! 2.3. ==> CREATION DE PLUSIEURS VECTEURS PAR TYPE DE MAILLE PRESENT :
!              UN VECTEUR CONTENANT LES NOMS DES MAILLES/TYPE
!           +  UN VECTEUR CONTENANT LES NUMEROS DES MAILLES/TYPE
!           +  UN VECTEUR CONTENANT LA CONNECTIVITE DES MAILLE/TYPE
!              (CONNECTIVITE = NOEUDS + UNE VALEUR BIDON(0) SI BESOIN)
!
    do 23 , ityp = 1, ntymax
!
    if (nmatyp(ityp) .ne. 0) then
!
        call wkvect('&&'//nompro//'.NOM.'//nomtyp(ityp), 'V V K16', nmatyp(ityp), jnomma(ityp))
        call wkvect('&&'//prefix//'.NUM.'//nomtyp(ityp), 'V V I', nmatyp(ityp), jnumma(ityp))
        call wkvect('&&'//nompro//'.CNX.'//nomtyp(ityp), 'V V I', nnotyp(ityp)*nmatyp(ityp),&
                    jcnxma(ityp))
!
    endif
!
    23 end do
!
! 2.4. ==> ON PARCOURT TOUTES LES MAILLES. POUR CHACUNE D'ELLES, ON
!          STOCKE SON NOM, SON NUMERO, SA CONNECTIVITE
!          LA CONNECTIVITE EST FOURNIE EN STOCKANT TOUS LES NOEUDS A
!          LA SUITE POUR UNE MAILLE DONNEE.
!          C'EST CE QU'ON APPELLE LE MODE ENTRELACE DANS MED
!          A LA FIN DE CETTE PHASE, NMATYP CONTIENT LE NOMBRE DE MAILLES
!          POUR CHAQUE TYPE
!
    do 241 , ityp = 1 , ntymax
    nmatyp(ityp) = 0
    241 end do
!
    do 242 , ima = 1, nbmail
!
    ityp = typma(ima)
!       ON TRAITE LES PENTA18 EN OUBLIANT
!       LES NOEUDS DU CENTRE ET LES SEG4 EN OUBLIANT
!       LES 2 NOEUDS CENTRAUX:
    if (ityp .eq. ipe18) ityp=ipe15
    ipoin = point(ima)
    nmatyp(ityp) = nmatyp(ityp) + 1
!       NOM DE LA MAILLE DE TYPE ITYP DANS VECT NOM MAILLES
    zk16(jnomma(ityp)-1+nmatyp(ityp)) = nommai(ima)//'        '
!                                                         12345678
!       NUMERO ASTER DE LA MAILLE DE TYPE ITYP DANS VECT NUM MAILLES
    zi(jnumma(ityp)-1+nmatyp(ityp)) = ima
!       CONNECTIVITE DE LA MAILLE TYPE ITYP DANS VECT CNX:
!       I) POUR LES TYPES DE MAILLE DONT LA NUMEROTATION DES NOEUDS
!          ENTRE ASTER ET MED EST IDENTIQUE:
    if (modnum(ityp) .eq. 0) then
        do 2421 , ino = 1, nnotyp(ityp)
        zi(jcnxma(ityp)-1+(nmatyp(ityp)-1)*nnotyp(ityp)+ino) =&
                connex(ipoin-1+ino)
2421      continue
!       II) POUR LES TYPES DE MAILLE DONT LA NUMEROTATION DES NOEUDS
!          ENTRE ASTER ET MED EST DIFFERENTE (CF LRMTYP):
    else
        do 2422 , ino = 1, nnotyp(ityp)
        zi(jcnxma(ityp)-1+(nmatyp(ityp)-1)*nnotyp(ityp)+ino) =&
                connex(ipoin-1+nuanom(ityp,ino))
2422      continue
    endif
!
    242 end do
!
!====
! 3. ECRITURE
!    ON PARCOURT TOUS LES TYPES POSSIBLES POUR MED ET ON DECLENCHE LES
!    ECRITURES SI DES MAILLES DE CE TYPE SONT PRESENTES DANS LE MAILLAGE
!    LA RENUMEROTATION PERMET D'ECRIRE LES MAILLES DANS L'ORDRE
!    CROISSANT DE LEUR TYPE MED. CE N'EST PAS OBLIGATOIRE CAR ICI ON
!    FOURNIT LES TABLEAUX DE NUMEROTATION DES MAILLES. MAIS QUAND CES
!    TABLEAUX SONT ABSENTS, C'EST LA LOGIQUE QUI PREVAUT. DONC ON LA
!    GARDE DANS LA MESURE OU CE N'EST PAS PLUS CHER ET QUE C'EST CE QUI
!    EST FAIT A LA LECTURE
!====
!
    do 31 , letype = 1 , nbtyp
!
! 3.0. ==> PASSAGE DU NUMERO DE TYPE MED AU NUMERO DE TYPE ASTER
!
    ityp = renumd(letype)
!
    if (infmed .ge. 2) then
        write (ifm,3001) nomtyp(ityp), nmatyp(ityp)
    endif
    3001 format('TYPE ',a8,' : ',i10,' MAILLES')
!
    if (nmatyp(ityp) .ne. 0) then
!
! 3.1. ==> LES CONNECTIVITES
!          LA CONNECTIVITE EST FOURNIE EN STOCKANT TOUS LES NOEUDS A
!          LA SUITE POUR UNE MAILLE DONNEE.
!          C'EST CE QUE MED APPELLE LE MODE ENTRELACE
!
        call mfcone(fid, nomamd, zi(jcnxma(ityp)), nnotyp(ityp)* nmatyp(ityp), edfuin,&
                    nmatyp(ityp), edmail, typgeo(ityp), ednoda, codret)
        if (codret .ne. 0) then
            saux08='MFCONE  '
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
!
! 3.2. ==> LE NOM DES MAILLES
!
        call mfnome(fid, nomamd, zk16(jnomma(ityp)), nmatyp( ityp), edmail,&
                    typgeo(ityp), codret)
        if (codret .ne. 0) then
            saux08='MFNOME  '
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
!
! 3.3. ==> LE NUMERO DES MAILLES
!
        call mfnume(fid, nomamd, zi(jnumma(ityp)), nmatyp(ityp), edmail,&
                    typgeo(ityp), codret)
        if (codret .ne. 0) then
            saux08='MFNUME  '
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
!
    endif
!
    31 end do
!
!====
! 4. LA FIN
!====
!
    do 41 , ityp = 1, ntymax
    if (nmatyp(ityp) .ne. 0) then
        call jedetr('&&'//nompro//'.NOM.'//nomtyp(ityp))
        call jedetr('&&'//nompro//'.CNX.'//nomtyp(ityp))
    endif
    41 end do
!
    call jedema()
!
end subroutine

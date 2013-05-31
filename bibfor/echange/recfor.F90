subroutine recfor(numpas, nbpal, force, typal, finpal,&
                  cnpal, prdeff, conv)
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
! person_in_charge: nicolas.greffet at edf.fr
! **********************************************************************
! *   LOGICIEL CODE_ASTER - COUPLAGE ASTER/EDYOS - COPYRIGHT EDF 2009  *
! **********************************************************************
!
!PAT_FONCTION
!
!  RECFOR : FONCTION
!  -----------------
!
!
!    CE SSP PERMET (DANS UNE BOUCLE SUR LES PALIERS) DE :
!            - LIRE LES EFFORTS EN PROVENANCE D'EDYOS
!            - SI TYPE PALIER = PAPANL LIRE LES VARIABLES RETOURNEES PAR
!      EDYOS (AU 16/02/09 ON NE SE SERT PAS DE CES VARIABLES DANS ASTER)
!              (ELLES SERVENT A CONSTITUER DES FICHIERS DE REPRIS =>
!              CETTE ECRITURE SERA SUPPRIMEE DANS EDYOS)
!            - IDEM SI TYPALIER = PACONL
!
!
!
! L'AXE DU ROTOR EST SUPPOSE ETRE EN Z (EN X DANS CADYRO)
!
!PAT_FONCTION
!
!=======================================================================
!
!  REFERENCES BIBLIOGRAPHIQUES
!  ---------------------------
!
!  NOTE HI-26/03/007A
!  "DEVELOPPEMENT D'UN MODE PRODUCTION POUR CALCIUM: MANUEL UTILISATEUR"
!  FAYOLLE ERIC, DEMKO BERTRAND (CS SI)  JUILLET 2003
!
!  LES APPELS YACS SONT STRICTEMENTS IDENTIQUES A CEUX DE CALCIUM A
!  L'EXCEPTION DU RAJOUT D'UN PREMIER ARGUMENT (ICOMPO) CORRESPONDANT A
!  UNE ADRESSE NECESSAIRE A L'EXECUTION DE YACS
!
! ======================================================================
!  DEVELOPPEMENTS ET CORRECTIONS D'ANOMALIES
!  -----------------------------------------
!  DATE: 13/02/09   AUTEUR: P. VAUGRANTE    ANOMALIE: DEVELOPPEMENT
!  DATE:            AUTEUR:                 ANOMALIE:
!  DATE:            AUTEUR:                 ANOMALIE:
!  DATE:            AUTEUR:                 ANOMALIE:
! ======================================================================
!
!  VARIABLES UTILISEES
!  -------------------
!
!  ____________________________________________________________________
! !    NOM    !   TYPE     !                 ROLE                      !
! !___________!____________!___________________________________________!
! !           !            !                                           !
! !           !            !                                           !
! ! NEQ       !  ENTIER    ! IN: DIMENSION (ENVOYEE PAR JEVEUX) POUR   !
! !           !            ! LES EFFORTS                               !
! !           !            !                                           !
! !           !            !                                           !
! ! NUMPAS    !  ENTIER    ! IN: NUMERO D'ITERATION                    !
! !           !            ! PERMET DE FAIRE CORRESPONDRE LES INSTANTS !
! !           !            ! POUR LES ECHANGES VIA YACS                !
! !           !            !                                           !
! ! NUMDDL    !  CHARACTER ! IN : NOM D'UN NUME_DDL OU D'UN CHAM_NO    !
! !           !            ! (VARIABLE ASTER) (='NDDL')                !
! !           !            !                                           !
! ! FORCE(NEQ)!  REEL*8    ! PARAMETRES DE TYPE REEL RECU D'EDYOS      !
! !           !            ! (REACTION EN X, REACTION EN Y)            !
! !           !            !                                           !
! ! COMP      !  CHARACTER ! NOM DES COMPOSANTES ASTER                 !
! !           !            !                                           !
! ! PARAMR(6) !  REEL*8    ! PARAMETRES DE TYPE REEL ENVOYES PAR EDYOS !
! !           !            ! (INDICE DE CONVERGENCE, REACTION EN X,    !
! !           !            !  REACTION EN Y,)      !
! !           !            !                                           !
! ! INFO      !  ENTIER    ! FLAG DE RETOUR DE YACS INDIQUANT SI LE    !
! !           !            ! TRANSFERT S'EST BIEN EFFECTUE (INFO=0)    !
! !           !            ! DANS LE CAS CONTRAIRE CE FLAG EST         !
! !           !            ! INTERPRETE PAR LE SSP ERRCOU              !
! !           !            !                                           !
! ! TR8       !  REEL*8    ! NE SERT A RIEN, C'EST JUSTE UNE VARIABLE  !
! !           !            ! NECESSAIRE POUR LES APPELS YACS           !
! !           !            ! (UTILE LORSQUE YACS UTILISE LE TEMPS      !
! !           !            ! POUR LA CORRESPONDANCE)                   !
! !           !            !                                           !
! ! NOMPRG    !  CHARACTER ! NOM DU SSP (POUR ECRITURE DANS ERRCOU)    !
! !           !            !                                           !
! ! NPAS      ! ENTIER     ! IN: NUMERO D'ITERATION EN I*4             !
! !           !            !                                           !
! ! IDIM      !  ENTIER    ! DIMENSION DE LA VARIABLE ECHANGEE         !
! !           !            ! (TELLE QUE PROGRAMMEE)                    !
! !           !            !                                           !
! ! NLU       !  ENTIER    ! DIMENSION DE LA VARIABLE ECHANGEE         !
! !           !            ! (RENVOYE PAR YACS)                        !
! !           !            !                                           !
! ! CFRTA     !  REEL*8    ! COUPLE DE FROTTEMENT TOTAL SUR L'ARBRE    !
! !           !            ! DIMENSIONNE (POUR PACONL)                 !
! !           !            ! (PAS UTILISE PAR ASTER LE 16/02/09)       !
! !           !            !                                           !
! ! PALMAX    !  ENTIER    ! NOMBRE MAXIMUM DE PALIERS                 !
! !           !            !                                           !
! !___________!____________!___________________________________________!
!
!
!
! INCLUDE CALCIUM.H
!  _____________________________________________________________________
! !         !             !                                            !
! ! LENVAR  !  ENTIER     !  LONGUEUR DES NOMS DES VARIABLES ECHANGEES !
! !         !             !                                            !
! ! NOMVAR  !  CHARACTER  !  NOM DE LA VARIABLE ECHANGEE AVEC EDYOS    !
! !         !  (*LENVAR)  !  (CE NOM ET SA CORESPONDACE EDYOS EST      !
! !         !             !  DEFINI DANS LES FICHIERS UTILISES PAR     !
! !         !             !  YACS : *.PY ET *.XML)                     !
! !         !             !                                            !
! ! CPITER  !  ENTIER     !  CORRESPOND A CPITER POUR YACS       !
! !         !             !  VAUT 41 ET SIGNIFIE QUE YACS FAIT         !
! !         !             !  CORRESPONDRE LES NUMEROS D'ITERATION      !
! !         !             !  ENTRE ASTER ET EDYOS (VOIR BIBLIOGRAPHIE) !
! !_________!_____________!____________________________________________!
!
!
!
!
! "COMMON" ASTER
! --------------
!
!  COMMON ZI (TYPE: INTEGER) (NOM = '&ADR_YACS')
!  ____________________________________________________________________
! !        !             !                                            !
! ! ICOMPO !  ADR        !  ADRESSE NECESSAIRE AUX APPELS YACS        !
! !________!_____________!____________________________________________!
!
!
!
!
!
!
!  COMMON ZI (TYPE: INTEGER) (NOM = 'N_PAL')
!  __________________________________________________________________
! !            !             !                                      !
! ! NBPAL      !  ADR        !  NOMBRE DE PALIERS POUR L'ETUDE      !
! !            !             !                                      !
! ! NOPAL(IPAL)!  ADR+1      !  NUMERO DU NOEUD ASTER POUR LE PALIER!
! !            !  +(IPAL-1)  !  CONSIDERE                           !
! !____________!_____________!______________________________________!
!
!
!
!  COMMON ZK8 (TYPE: CHARACTER*8) (NOM = 'C_PAL')
!  ____________________________________________________________________
! !             !             !                                       !
! ! TYPAL(IPAL) ! ADR+(IPAL-1)!  TYPE DU PALIER CONSIDERE             !
! !             !             !                                       !
! ! FINPAL(IPAL)!  ADR+PALMAX !  TERMINAISON POUR LE PALIER CONSIDERE !
! !             !  +(IPAL-1)  !  PALIER N°I => _I                     !
! !             !             !                                       !
! ! CNPAL(IPAL) ! ADR+2*PALMAX!  NOM DU NOEUD ASTER POUR LE PALIER    !
! !             !  +(IPAL-1)  !  CONSIDERE                            !
! !_____________!_____________!_______________________________________!
!
!
!
!=======================================================================
!  SOUS PROGRAMME(S) APPELE(S) : CP* (YACS), ERRCOU.F
!
!-----------------------------------------------------------------------
!  SOUS PROGRAMME(S) APPELANT(S) :  OP0115.F, OP0111.F
!
!***********************************************************************
!%W% %D% %T%
!
!
!
! aslint: disable=,W1304
    implicit none
!
!     ARGUMENTS
!     =========
    include 'jeveux.h'
    include 'asterc/cpldb.h'
    include 'asterfort/errcou.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mess.h'
    integer :: numpas, nbpal
    real(kind=8) :: force(nbpal, *), conv
!
!
!
!
!
!     VARIABLES INTERNES
!     ==================
    integer :: ifm, niv
    character(len=8) :: nomprg
    parameter(nomprg='RECFOR')
!
    integer(kind=4) :: ipat, ipal, info, idim, npas, nlu
    real(kind=8) :: paramr(6), tr8
!
!     REPRISE NON PROGRAMMEE AU 16/02/09 MAIS PAR CONCORDANCE EDYOS
!      (PAPANL, PACONL)
    logical :: repris
    parameter(repris=.false.)
!
!     A SUPPRIMER QUAND REPRIS = FALSE
    integer(kind=4) :: nsmax
    parameter (nsmax = 24 )
    real(kind=8) :: mvtpat (6, nsmax), mvtcon (6, nsmax)
!
!
!     ANCIENS INCLUDE (CALCIUM.H)
!     ===========================
    integer(kind=4) :: lenvar
    parameter (lenvar = 144)
    character*(lenvar) :: nomvar
    integer(kind=4) :: cpiter
    parameter (cpiter= 41)
!
!
!     =================================
    integer :: icompo
    integer :: palmax
    parameter (palmax=20)
    integer :: iadr
    character(len=3) :: finpal(palmax)
    character(len=6) :: typal(palmax)
    character(len=8) :: cnpal(palmax)
    character(len=24) :: ayacs
!
    logical :: prdeff
!
!     DEBUT DU SSP
!     ============
    call jemarq()
    niv = 0
    call infdbg('YACS_EDYOS', ifm, niv)
!
!     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
!     ------------------------------------------------------------
    ayacs='&ADR_YACS'
    conv = 1.d0
!     RECUPERATION DES DONNEES DANS LES "COMMON" ASTER
!     ================================================
!
!
!     RECUPERATION DE L'ADRESSE YACS
!     ------------------------------
    call jeveuo(ayacs, 'L', iadr)
    icompo=zi(iadr)
!      BOUCLE SUR LES PALIERS (NOEUDS ASTER)
    do 10 ipal = 1, nbpal
        force(ipal,1) = 0.d0
        force(ipal,2) = 0.d0
        force(ipal,3) = 0.d0
!
!        LECTURE DES EFFORTS EN PROVENANCE D'EDYOS
!        -----------------------------------------
        nomvar = 'FORCETE'//finpal(ipal)
        tr8=0.d0
        if (typal(ipal) .ne. 'PACONL') then
            idim=3
        else
            idim=4
        endif
        do 20 ipat = 1, 6
            paramr(ipat)=0.d0
20      continue
        npas=numpas
        call cpldb(icompo, cpiter, tr8, tr8, npas,&
                   nomvar, idim, nlu, paramr, info)
        call errcou(nomprg, npas, nomvar, info, idim,&
                    nlu)
!         ECRITURE DES VALEURS RECUES
        if (niv .ge. 2) then
            write(ifm,*)'==== ASTEREDYOS :',nomprg,' NUMPAS =======',&
            numpas
            write(ifm,*)'ASTEREDYOS: ',nomprg,&
     &        ' ASTER  LECTURE EFFORTS EDYOS'
            write(ifm,*)'ASTEREDYOS: ',nomprg,' NUMERO PALIER: ',&
            ipal
            write(ifm,*)'ASTEREDYOS: ',nomprg,' TYPE PALIER: ',&
            typal(ipal)
            write(ifm,*)'ASTEREDYOS: ',nomprg,' NOEUD PALIER: ',&
            cnpal(ipal)
            write(ifm,*)'ASTEREDYOS: ',nomprg,' CONVERGENCE: ',paramr(&
            1)
            write(ifm,*)'ASTEREDYOS: ',nomprg,' WX : ',paramr(2)
            write(ifm,*)'ASTEREDYOS: ',nomprg,' WY : ',paramr(3)
            write(ifm,*)'ASTEREDYOS: ',nomprg,' PACONL: PARAMR(4): ',&
            paramr(4)
            write(ifm,*)'ASTEREDYOS: '
            write(ifm,*)'=========  ASTEREDYOS :  ',nomprg,'   ========'
        endif
!
!         VERIFICATION DE LA CONVERGENCE
!         ------------------------------
        conv = paramr(1)
        if (conv .le. 0.d0) call u2mess('A', 'EDYOS_45')
!
!         AFFECTATION DES EFFORTS RECU D'EDYOS
!         ------------------------------------
!
!         REACTION HYDRAULIQUE EN X (CORRESPOND AU Z DE EDYOS)
!          ICOMP=N_DDL(IPAL,1)
        force(ipal,1) = paramr(2)
!
!         REACTION HYDRAULIQUE EN Y
!          ICOMP=N_DDL(IPAL,2)
        force(ipal,2) = paramr(3)
!          IF(TYPAL(IPAL) .EQ. 'PACONL')THEN
!            CFRTA = PARAMR(4)
!          ENDIF
!         EN CAS DE REPRIS ULTERIEURE (A SUPPRIMER)
!         ------------------------------------------
!         (AU 16/02/09 ON NE FAIT RIEN AVEC CES DONNEES JUSTE POUR LA
!             CONCORDANCE AVEC EDYOS)
!
        if (repris) then
            idim=6*nsmax
            nomvar = 'REPRISEASTER'//finpal(ipal)
!            LECTURE DES DONNEES ASSOCIES AUX PATINS (PAPANL)
!            ------------------------------------------------
            if (typal(ipal) .eq. 'PAPANL') then
                if (prdeff) then
                    if (niv .ge. 2) write(ifm, * )'ASTEREDYOS : PAPANL - NOMVAR ', nomvar
                    npas=numpas
                    call cpldb(icompo, cpiter, tr8, tr8, npas,&
                               nomvar, idim, nlu, mvtpat, info)
                    call errcou(nomprg, npas, nomvar, info, idim,&
                                nlu)
                endif
            endif
!            LECTURE DES DONNEES ASSOCIES A PACONL
!            -------------------------------------
            if (typal(ipal) .eq. 'PACONL') then
                if (prdeff) then
                    if (niv .ge. 2) write(ifm, * )'ASTEREDYOS : PAPANL - NOMVAR ', nomvar
                    npas=numpas
                    call cpldb(icompo, cpiter, tr8, tr8, npas,&
                               nomvar, idim, nlu, mvtcon, info)
                    call errcou(nomprg, npas, nomvar, info, idim,&
                                nlu)
                endif
            endif
        endif
!         FIN DE CONSTITUTION DU FICHIER EN CAS DE REPRISE
10  end do
!      FIN DE LA BOUCLE SUR LES PALIERS
    call jedema()
!
end subroutine

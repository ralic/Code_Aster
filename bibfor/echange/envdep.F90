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
!  ENVDEP : FONCTION
!  -----------------
!
! CE SSP PERMET D'ENVOYER A EDYOS LES DEPLACEMENTS ET VITESSES CALCULES
! PAR ASTER.
! AU PREMIER PAS DE TEMPS, ON ENVOIT DES DEPLACEMENTS ET VITESSES NULS
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
!  _____________________________________________________________________
! !    NOM      !   TYPE      !                  ROLE                  !
! !_____________!_____________!________________________________________!
! !             !             !                                        !
! ! TEMPS       !  REEL*8     !  IN: INSTANT COURANT                   !
! !             !             !                                        !
! ! DT          !  REEL*8     !  IN: PAS DE TPS ASTER (EDYOS) INITIAL  !
! !             !             !                                        !
! ! DTSTO       !  REEL*8     !  IN: PAS DE TEMPS POUR STOCKAGE EDYOS  !
! !             !             !                                        !
! ! NEQ         !  ENTIER     !  IN: DIMENSION (PAR JEVEUX) POUR       !
! !             !             !  LES DEPLACEMENTS ET LES VITESSES      !
! !             !             !                                        !
! ! DEP(NEQ)    !  REEL*8     !  IN: VECTEUR DES DEPLACEMENTS GLOBAUX  !
! !             !             !                                        !
! ! VIT(NEQ)    !  REEL*8     !  IN: VECTEUR DES VITESSES GLOBALES     !
! !             !             !                                        !
! ! NUMPAS      !  ENTIER     !  IN: NUMERO D'ITERATION                !
! !             !             !  FAIT CORRESPONDRE LES INSTANTS        !
! !             !             !  POUR LES ECHANGES VIA YACS            !
! !             !             !                                        !
! ! NUMDDL      !  CHARACTER  !  IN: NOM D'UN NUME_DDL OU D'UN CHAM_NO !
! !             !             !  (VARIABLE ASTER) (='NDDL')            !
! !             !             !                                        !
! ! COMP        !  CHARACTER  !  NOM DES COMPOSANTES ASTER             !
! !             !             !                                        !
! ! VROTAT      !  REEL*8     !  IN: VITESSE DE ROT. INITIALE DU ROTOR !
! !             !             !                                        !
! ! RZPR        !  REEL*8     !  VARIABLE NON UTILISEE PAR EDYOS (=> 0)!
! !             !             !                                        !
! ! POSVIT(11)  !  REEL*8     !  PARAMETRES DE TYPE R ENVOYES A EDYOS  !
! !             !             !  (TEMPS, DT, DEP_X, DEP_Y, VIT_X,VIT_Y !
! !             !             !   DEP_RX, DEP_RY, RZPR, vit_rot,dtsto) !
! !             !             !                                        !
! ! INFO        !  ENTIER     !  FLAG DE RETOUR DE YACS INDIQUANT SI LE!
! !             !             !  TRANSFERT S'EST BIEN EFFECTUE (INFO=0)!
! !             !             !  DANS LE CAS CONTRAIRE CE FLAG EST     !
! !             !             !  INTERPRETE PAR LE SSP ERRCOU          !
! !             !             !                                        !
! ! TR8         !  REEL*8     !  NE SERT A RIEN, C'EST JUSTE UNE VAR.  !
! !             !             !  NECESSAIRE POUR LES APPELS YACS       !
! !             !             !  (UTILE LORSQUE YACS UTILISE LE TEMPS  !
! !             !             !  POUR LA CORRESPONDANCE)               !
! !             !             !                                        !
! ! NOMPRG      !  CHARACTER  !  NOM DU SSP (POUR ECRITURE DANS ERRCOU)!
! !             !             !                                        !
! !             !             !                                        !
! !_____________!_____________!________________________________________!
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
! ! CPITER  !  ENTIER     !  CORRESPOND A CP_ITERATION POUR YACS       !
! !         !             !  VAUT 41 ET SIGNIFIE QUE YACS FAIT         !
! !         !             !  CORRESPONDRE LES NUMEROS D'ITERATION      !
! !         !             !  ENTRE ASTER ET EDYOS (VOIR BIBLIOGRAPHIE) !
! !_________!_____________!____________________________________________!
!
!
! COMMON YACS
!  _____________________________________________________________________
! !         !             !                                            !
! ! ICOMPO  ! ENTIER*8    ! ADRESSE NECESSAIRE AUX APPELS YACS         !
! !         !             !                                            !
! !         !             !                                            !
! !_________!_____________!____________________________________________!
!
!
! COMMON CADY
!  _____________________________________________________________________
! !         !             !                                            !
! ! NBPAL   ! ENTIER      ! IN : NOMBRE DE PALIERS                     !
! !         !             !                                            !
! ! IFM     ! ENTIER      ! IN : UNITE LOGIQUE POUR L'ECRITURE         !
! !         !             !     (PRINCIPALEMENT POUR LE DEBUGGAGE)     !
! !         !             !                                            !
! !_________!_____________!____________________________________________!
!
!
! COMMON PALIERS
!  _____________________________________________________________________
! !             !             !                                        !
! !             !             !                                        !
! !FINPAL(NBPAL)!  CHARACTER*3!  IN : TERMINAISON POUR LES PALIERS     !
! !             !             !  PALIER N°I => _I                      !
! !             !             !                                        !
! ! PALMAX      !  ENTIER     !  NOMBRE MAXIMUM DE PALIERS             !
! !             !             !                                        !
! ! TYPE_PAT    !  CHARACTER*6!TYPE DES PALIERS (NON UTILISE DS INIPAT)!
! !             !  (PALMAX)   !                                        !
! !             !             !                                        !
! !             !             !                                        !
! !_____________!_____________!________________________________________!
!
!
!
! COMMON NOEUDS_PALIERS
!  ________________________________________________________________
! !                  !             !                               !
! ! CNPAL(PALMAX)    !  CHARACTER  !  NOM DU NOEUD ASTER           !
! !                  !             !                               !
! ! NOPAL(PALMAX)    !  ENTIER     !  NUMERO DU NOEUD ASTER        !
! !                  !             !                               !
! ! DIMNAS           !  CHARACTER  !  NOMBRE DE DDL POUR UN NOEUD  !
! !                  !             !                               !
! !__________________!_____________!_______________________________!
!
!
! "COMMON" ASTER
! --------------
!
!  COMMON ZI (TYPE: INTEGER) (NOM = '&ADR_YACS')
!  _______________________________________________________________
! !          !             !                                     !
! ! ICOMPO   !  ADR        !  ADRESSE NECESSAIRE AUX APPELS YACS !
! !__________!_____________!_____________________________________!
!
!
!
!
!
!
!  COMMON ZI (TYPE: INTEGER) (NOM = 'N_PAL')
!  ___________________________________________________________________
! !            !             !                                       !
! ! NBPAL      !  ADR        !  NOMBRE DE PALIERS POUR L'ETUDE       !
! !            !             !                                       !
! ! NOPAL(IPAL)!  ADR+1      !  NUMERO DU NOEUD ASTER POUR LE PALIER !
! !            !  +(IPAL-1)  !  CONSIDERE                            !
! !____________!_____________!_______________________________________!
!
subroutine envdep(numpas, nbpal, dt, dtsto, temps,&
                  dep, vit, vrotat, finpal, prdeff)
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
! aslint: disable=,,W1304
    implicit none
!
!
!     ARGUMENTS
!     =========
    include 'jeveux.h'
    include 'asterc/cpedb.h'
    include 'asterc/r8pi.h'
    include 'asterfort/errcou.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: numpas, nbpal
    real(kind=8) :: temps, dep(nbpal, *), vit(nbpal, *), dt, dtsto, vrotat
    logical :: prdeff
!
!
!
!
!
!     VARIABLES INTERNES
!     ==================
    integer :: ifm, niv
    character(len=8) :: nomprg
    parameter(nomprg='ENVDEP')
!
    integer(kind=4) :: ipat, ipal, info, npas
    real(kind=8) :: vitpal(6), deppal(6), tr8
    real(kind=8) :: posvit(11), rzpr
    integer(kind=4) :: onze
    parameter  (onze=11)
    real(kind=8) :: pi
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
!     COMMON
!     ======
    integer :: icompo
    integer :: palmax
    parameter (palmax=20)
    integer :: iadr
    character(len=3) :: finpal(palmax)
    character(len=24) :: ayacs
!
!     DEBUT SSP
!     =========
!
    call jemarq()
    niv = 0
    call infdbg('YACS_EDYOS', ifm, niv)
!
!     initialisation de pi
!     --------------------
    pi = r8pi()
!
!     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
!     ------------------------------------------------------------
    ayacs='&ADR_YACS'
!
!     RECUPERATION DES DONNEES DANS LES "COMMON" ASTER
!     ================================================
!
!     RECUPERATION DE L'UNITE D'ECRITURE
!     ----------------------------------
!
!     RECUPERATION DE L'ADRESSE YACS
!     ------------------------------
    call jeveuo(ayacs, 'L', iadr)
    icompo=zi(iadr)
!
!     RZPR: ROTATION INITIALE (PAS UTILISÉE PAR EDYOS (XAV))
!     ------------------------------------------------------
    rzpr =0.d0
!
!     REMPLISSAGE DU VECTEUR POSVIT POUR EDYOS
!     ----------------------------------------
    posvit(1)=temps
    posvit(2)=dt
!
!     DEBUT DE LA BOUCLE SUR LES PALIERS
!
    do 10 ipal = 1, nbpal
!
!      SUBROUTINE POSDDL ( TYPE, RESU, NOEUD, CMP, NUNOE, NUDDL )
!     ------------------------------------------------------------------
!     DONNE LE NUMERO DU NOEUD
!           NUNOE = 0 SI LE NOEUD N'EXISTE PAS
!     DONNE LE NUMERO DU DDL ASSOCIE AU NOEUD ET A SA COMPOSANTE
!           NUDDL = 0 SI LE COUPLE (NOEUD,COMPOSANTE) N'EXISTE PAS
!     ------------------------------------------------------------------
! IN  TYPE   : TYPE DU RESU
! IN  RESU   : NOM D'UN NUME_DDL OU D'UN CHAM_NO
! IN  NOEUD  : NOM DU NOEUD
! IN  CMP    : NOM DE LA COMPOSANTE
! OUT NUNOE  : NUMERO LOCAL DU NOEUD
! OUT NUDDL  : NUMERO DU DDL ASSOCIE AU NOEUD DE COMPOSANTE CMP
!     ------------------------------------------------------------------
!
        if (numpas .le. 1) then
!            PREMIER PAS DE TEMPS => DEPLACEMENTS ET VITESSES NULS
            posvit(3)=0.d0
            posvit(4)=0.d0
            posvit(5)=0.d0
            posvit(6)=0.d0
            posvit(7)=0.d0
            posvit(8)=0.d0
            posvit(9)=rzpr
            posvit(10) = (vrotat/30.d0)*pi
            if (niv .ge. 2) write(ifm, *)' ASTEREDYOS : ', nomprg, ' VIT_ROTAT LUE ', posvit(10)
!
            posvit(11)=dtsto
!
        else
            do 20 ipat = 1, 6
                deppal(ipat)=dep(ipal,ipat)
                vitpal(ipat)=vit(ipal,ipat)
20          continue
            posvit(3)=deppal(1)
            posvit(4)=deppal(2)
            posvit(5)=vitpal(1)
            posvit(6)=vitpal(2)
            posvit(7)=deppal(4)
            posvit(8)=deppal(5)
            posvit(9)=rzpr
            posvit(10)=(vrotat/30.d0)*pi
            posvit(11)=dtsto
        endif
!
!         ECRITURE DES VALEURS ENVOYEES
        if (niv .ge. 2) then
            write(ifm,*)' *****  ASTEREDYOS :',nomprg,'  NUMPAS = ',&
     &           numpas
            write(ifm,*)' ASTEREDYOS : ',nomprg,&
     &           '  : ENVOI DES DEPLA A EDYOS'
            write(ifm,*)' ASTEREDYOS : ',nomprg,&
     &           '  - NUMERO PALIER : ',ipal
            write(ifm,*)' ASTEREDYOS : ',nomprg,&
     &           '  TEMPS,PAS DE TEMPS: ',posvit(1),posvit(2)
            write(ifm,*)' ASTEREDYOS : ',nomprg,'  DEPX  DEPY: ',&
     &           posvit(3),posvit(4)
            write(ifm,*)' ASTEREDYOS : ',nomprg,'  VITX  VITY: ',&
     &           posvit(5),posvit(6)
            write(ifm,*)' ASTEREDYOS : ',nomprg,'  ROTX  ROTY  ',&
     &           posvit(7),posvit(8)
            write(ifm,*)' ASTEREDYOS : ',nomprg,&
     &           '  RZPR (PAS UTILISEE): ',posvit(9)
            write(ifm,*)' ASTEREDYOS : ',nomprg,'  OMEGA : ',&
     &           posvit(10)
            write(ifm,*)' ASTEREDYOS : ',nomprg,&
     &           '  PAS DE TEMPS DE STOCKAGE:',posvit(11)
            write(ifm,*)' ASTEREDYOS : ',nomprg,'  ITERATION : ',&
     &           numpas
            write(ifm,*)' *****  ASTEREDYOS : ',nomprg,'    ****** '
        endif
!
!
!         TR8 = TEMPS PIPEAU NE SERVANT A RIEN
        tr8 = 0.d0
        nomvar='POSITION'//finpal(ipal)
!
!         ENVOI DES DEPLACEMENTS ET VITESSES A EDYOS
        npas=numpas
        if (prdeff) then
            call cpedb(icompo, cpiter, tr8, npas, nomvar,&
                       onze, posvit, info)
            call errcou(nomprg, npas, nomvar, info, onze,&
                        onze)
        endif
10  end do
!     FIN DE LA BOUCLE SUR LES PALIERS
    call jedema()
!
end subroutine

subroutine inicou(nbpas, tinit, tfin, dt, dtsto,&
                  vrotat)
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
!
!  INICOU : FONCTION
!
!    CE SSP PERMET (EN DEHORS DES BOUCLES TEMPORELLES) DE :
!        - INITIALISER LA LIAISON AVEC LE COUPLEUR
!
!        - DANS UNE BOUCLE SUR LES PALIERS
!            - ENVOYER (A EDYOS)LE NOM DU PALIER CONSIDERE
!
!            - RECEVOIR (D'EDYOS)LE TYPE DU PALIER CONSIDERE
!
!            - D'ENVOYER LES PARAMETRES REELS INITIAUX (TEMPS INITIAL
!              ET FINAL, PAS DE TEMPS, PAS DE TEMPS DE STOCKAGE,
!              VITESSE DE ROTATION EN TOURS/MINUTE)
!
!
!            - D'ENVOYER LES PARAMETRES ENTIERS INITIAUX (NOMBRE DE
!              PAS DE TEMPS EDYOS (VOIR COHERENCE AVEC VALEURS
!              PRECEDENTES), 0 ?)
!
!
!     INICOU ECHANGE UNIQUEMENT AVEC INITCO (EDYOS)
!
!
!  VARIABLES UTILISEES
!  -------------------
!
!  ____________________________________________________________________
! !    NOM      !   TYPE    !                 ROLE                     !
! !_____________!___________!__________________________________________!
! !             !           !                                          !
! ! NBPAS       !  ENTIER*8 ! IN: NOMBRE DE PAS DE TEMPS TOTAL         !
! !             !           !                                          !
! ! TINIT       !  REEL*8   ! IN: INSTANT INITIAL                      !
! !             !           !                                          !
! ! TFIN        !  REEL*8   ! IN: INSTANT FINAL                        !
! !             !           !                                          !
! ! DT          !  REEL*8   ! IN: PAS DE TEMPS ASTER (ET EDYOS) INITIAL!
! !             !           !                                          !
! ! DTSTO       !  REEL*8   ! IN: PAS DE TEMPS POUR LE STOCKAGE EDYOS  !
! !             !           !                                          !
! ! VROTAT      !  REEL*8   ! IN: VITESSE DE ROTATION (TR/MN) DU ROTOR !
! !             !           !                                          !
! ! NUMPAS      !  ENTIER   ! NUMERO D'ITERATION (1 DANS CE SSP)       !
! !             !           ! PERMET DE FAIRE CORRESPONDRE LES INSTANTS!
! !             !           ! POUR LES ECHANGES VIA YACS               !
! !             !           !                                          !
! ! NLU         !  ENTIER   ! DIMENSION DE LA VARIABLE ECHANGEE        !
! !             !           ! (RENVOYE PAR YACS)                       !
! !             !           !                                          !
! ! UN,DEUX,CINQ!  ENTIER   ! DIMENSION DE LA VARIABLE ECHANGEE        !
! !             !           ! (TELLE QUE PROGRAMMEE DANS LE SSP)       !
! !             !           !                                          !
! ! NOMPAL      !CHARACTER*8! NOM DU PALIER CONSIDERE (ENVOYE A EDYOS) !
! !             !           !                                          !
! ! TYPPAL      !CHARACTER*6! TYPE DU PALIER CONSIDERE (RECU D'EDYOS)  !
! !             !           !                                          !
! ! PARAMR(6)   !  REEL*8   ! PARAMETRES DE TYPE REEL ENVOYES A EDYOS  !
! !             !           ! (TINIT, TFIN, DT, DTSTO, VROTAT)         !
! !             !           !                                          !
! ! PARAMI(2)   !  ENTIER   ! PARAMETRES DE TYPE ENTIER ENVOYES A EDYOS!
! !             !           ! (NBPAS, FLAG)                            !
! !             !           ! LE FLAG EST UN ENTIER (SI = 0 => CALCUL  !
! !             !           ! NORMAL, SI > 0 => REPRISE DE CALCUL)     !
! !             !           !                                          !
! ! INFO        !  ENTIER   ! FLAG DE RETOUR DE YACS INDIQUANT SI LE   !
! !             !           ! TRANSFERT S'EST BIEN EFFECTUE (INFO=0)   !
! !             !           ! DANS LE CAS CONTRAIRE CE FLAG EST        !
! !             !           ! INTERPRETE PAR LE SSP ERRCOU             !
! !             !           !                                          !
! ! TR8         !  REEL*8   ! NE SERT A RIEN, C'EST JUSTE UNE VARIABLE !
! !             !           ! NECESSAIRE POUR LES APPELS YACS          !
! !             !           ! (UTILE LORSQUE YACS UTILISE LE TEMPS     !
! !             !           ! POUR LA CORRESPONDANCE - VOIR CPITER)    !
! !             !           !                                          !
! ! TR4         !  REEL*4   ! NE SERT A RIEN, C'EST JUSTE UNE VARIABLE !
! !             !           ! NECESSAIRE POUR LES APPELS YACS          !
! !             !           ! (UTILE LORSQUE YACS UTILISE LE TEMPS     !
! !             !           ! POUR LA CORRESPONDANCE - VOIR CPITER)    !
! !             !           !                                          !
! ! PALMAX      !  ENTIER   ! NOMBRE MAXIMUM DE PALIERS (PARAMETER)    !
! !             !           !                                          !
! ! NOMPRG      !  CHARACTER! NOM DU SSP (POUR ECRITURE DANS ERRCOU)   !
! !             !           !                                          !
! !             !           !                                          !
! !_____________!___________!__________________________________________!
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
!
!
! "COMMON" ASTER
! --------------
!
!  COMMON ZI (TYPE: INTEGER) (NOM = '&ADR_YACS')
!  _____________________________________________________________________
! !          !             !                                           !
! ! ICOMPO   !  ADR        !  ADRESSE NECESSAIRE AUX APPELS YACS       !
! !__________!_____________!___________________________________________!
!
!
!
!
!
!
!  COMMON ZI (TYPE: INTEGER) (NOM = 'N_PAL')
!  _____________________________________________________________________
! !             !             !                                        !
! ! NBPAL       !  ADR        !  NOMBRE DE PALIERS POUR L'ETUDE        !
! !             !             !                                        !
! ! NOPAL(IPAL) !  ADR+1      !  NUMERO DU NOEUD ASTER POUR LE PALIER  !
! !             !  +(IPAL-1)  !  CONSIDERE                             !
! !_____________!_____________!________________________________________!
!
!
!
!  COMMON ZK8 (TYPE: CHARACTER*8) (NOM = 'C_PAL')
!  _____________________________________________________________________
! !               !             !                                      !
! ! TYPAL(IPAL)   ! ADR+(IPAL-1)!  TYPE DU PALIER CONSIDERE            !
! !               !             !                                      !
! ! FINPAL(IPAL)  !  ADR+PALMAX !  TERMINAISON POUR LE PALIER CONSIDERE!
! !               !  +(IPAL-1)  !  PALIER N°I => _I                    !
! !               !             !                                      !
! ! CNPAL(IPAL)   ! ADR+2*PALMAX!  NOM DU NOEUD ASTER POUR LE PALIER   !
! !               !  +(IPAL-1)  !  CONSIDERE                           !
! !_______________!_____________!______________________________________!
!
!
! aslint: disable=W1304,W1305
    implicit none
!
!     ARGUMENTS
!     =========
    include 'jeveux.h'
!
    include 'asterc/cpech.h'
    include 'asterc/cpedb.h'
    include 'asterc/cpeen.h'
    include 'asterc/cplch.h'
    include 'asterfort/errcou.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    real(kind=8) :: vrotat, tinit, tfin, dt, dtsto, tmin
    integer :: nbpas
!
!     COMMON
!     ======
    integer :: icompo
    integer :: nbpal
!
    integer :: palmax
    parameter (palmax=20)
    character(len=3) :: finpal(palmax)
!     VARIABLES INTERNES
!     ==================
    character(len=8) :: nomprg
    parameter(nomprg='INICOU')
!
    integer :: ifm, niv, iapp
    integer(kind=4) :: numpas, info, nlu, parami(2)
    integer(kind=4) :: un, deux, six
    parameter (un = 1, deux=2, six=6)
    real(kind=4) :: tr4
    real(kind=8) :: tr8, paramr(six)
    character(len=6) :: typpal
    character(len=8) :: nompal
!
!     ANCIENS INCLUDE (CALCIUM.H)
!     ===========================
    integer(kind=4) :: lenvar
    parameter (lenvar = 144)
    character(len=lenvar) :: nomvar
    integer(kind=4) :: cpiter
    parameter (cpiter= 41)
!
    integer :: iadr, iadri, iadrk
    character(len=24) :: cpal, npal, ayacs
!
!
!======================================================================
!
    tmin=1.d-11
!
    call jemarq()
    niv = 0
    call infdbg('YACS_EDYOS', ifm, niv)
!
!
!     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
!     ------------------------------------------------------------
    cpal='C_PAL'
    npal='N_PAL'
    ayacs='&ADR_YACS'
!
!     APPEL DU SSP DE LECTURE DES DONNEES (SUR LES PALIERS)
!      => COMMON PALIERS
!     ----------------------------------------------------------------
!
!     RECUPERATION DES DONNEES DANS LES "COMMON" ASTER
!     ================================================
!
!
!     RECUPERATION DE L'ADRESSE YACS
!     ------------------------------
    call jeveuo(ayacs, 'L', iadr)
    icompo=zi(iadr)
!
!
!     RECUPERATION DES DONNEES ENTIERES SUR LES PALIERS
!     -------------------------------------------------
    call jeveuo(npal, 'L', iadri)
    nbpal=zi(iadri)
!
!
!     RECUPERATION DES DONNEES ENTIERES SUR LES PALIERS
    call jeveuo(cpal, 'L', iadrk)
    do 20 iapp = 1, nbpal
        finpal(iapp) = zk8(iadrk+(iapp-1)+palmax)(1:3)
20  end do
!
!
!
!
!------------------------------ECRITURE NOM PALIER----------------------
!
    numpas = 1
    tr4 = 0.d0
    tr8 = 0.d0
!
    if (niv .ge. 2) write(ifm, *)'ASTEREDYOS: ', nomprg, '  NUM_PAS = ', numpas, ' ==='
!
!
!     DEBUT DE LA BOUCLE SUR LES PALIERS
    do 100 iapp = 1, nbpal
!
!        CREATION DU NOM DE VARIABLE YACS POUR LE NOM DU PALIER
!
        nompal = 'ANA1'//finpal(iapp)
        nomvar = 'NOM_'//nompal
!
!        --------   ENVOI DU NOM DU PALIER A EDYOS -------------
!
        if (niv .ge. 2) then
            write(ifm,'(A23,3X,A12)')'ASTEREDYOS: VARIABLE1: ',nomvar
            write(ifm,'(A23,3X,A12)')'ASTEREDYOS: NOMPALIER: ',nompal
        endif
        info = 0
        call cpech(icompo, cpiter, tr4, numpas, nomvar,&
                   un, nompal, info)
        call errcou(nomprg, numpas, nomvar, info, un,&
                    un)
!
        if (niv .ge. 2) then
            write(ifm,*)' '
            write(ifm,*)' '
            write(ifm,*)'ASTEREDYOS: ',nomprg,' PALIER SEND : ',&
            nompal(1:8)
        endif
!
!
!        CREATION DU NOM DE VARIABLE YACS POUR LE TYPE DU PALIER
!
        nomvar='TYPE_'//nompal
        typpal='      '
!
!
!        ----------  RECEPTION DU TYPE DU PALIER FROM EDYOS ----------
!
        call cplch(icompo, cpiter, tr4, tr4, numpas,&
                   nomvar, un, nlu, typpal, info)
        call errcou(nomprg, numpas, nomvar, info, un,&
                    nlu)
!
!
        if (niv .ge. 2) then
            write(ifm,*)'ASTEREDYOS: ',nomprg,' TYPE DU PALIER: ',&
            typpal(1:6)
        endif
!
!        -------ENVOI DES PARAMETRES REELS A EDYOS------------
!
!
        paramr ( 1 ) = tinit
        paramr ( 2 ) = tfin
        paramr ( 3 ) = dt
        paramr ( 4 ) = dtsto
        paramr ( 5 ) = vrotat
        paramr ( 6 ) = tmin
        dtsto=dt
        nomvar='PARAMREEL'//finpal(iapp)
!
        call cpedb(icompo, cpiter, tr8, numpas, nomvar,&
                   six, paramr, info)
        call errcou(nomprg, numpas, nomvar, info, six,&
                    six)
!
!
!        ECRITURE DES PARAMETRES ENVOYES
        if (niv .ge. 0) then
            write(ifm,*)'ASTEREDYOS :',nomprg,&
     &       '- ASTER - ENVOI PARAMR A EDYOS'
            write(ifm,*)'ASTEREDYOS : ',nomprg,' - TEMPS INITIAL : ',&
            paramr(1)
            write(ifm,*)'ASTEREDYOS : ',nomprg,' - TEMPS FINAL : ',&
            paramr(2)
            write(ifm,*)'ASTEREDYOS : ',nomprg,' - PAS DE TEMPS : ',&
            paramr(3)
            write(ifm,*)'ASTEREDYOS : ',nomprg,' - PAS STOCKAGE : ',&
            paramr(4)
            write(ifm,*)'ASTEREDYOS : ',nomprg,' - OMEGA : ',paramr(5)
            write(ifm,*)'ASTEREDYOS : ',nomprg,' - PAS MINIMUM  : ',&
     &        paramr(6)
        endif
!
!
!
!        -----------ENVOI PARAMETRES ENTIERS A EDYOS ----------------
!
!
        nomvar='PARAMENTI'//finpal(iapp)
!
        parami(1)=int(nbpas,4)
!         CALCUL NORMAL (PAS DE REPRISE) => PARAMI(2) = 0
        parami(2)=0
!
        call cpeen(icompo, cpiter, tr4, numpas, nomvar,&
                   deux, parami(1), info)
        call errcou(nomprg, numpas, nomvar, info, deux,&
                    deux)
!
!
!        ECRITURE DES PARAMETRES ENVOYES
        if (niv .ge. 2) then
            write(ifm,*)'ASTEREDYOS :',nomprg,&
     &         '- ASTER - ENVOI PARAMI A EDYOS'
            write(ifm,*)'ASTEREDYOS : ',nomprg,' - NBPAS : ',parami(1)
            write(ifm,*)'ASTEREDYOS : ',nomprg,' - PARAMI(2) (=0) :',&
            parami(2)
            write(ifm,*)'** ASTEREDYOS: FIN DE ',nomprg,&
     &          ' POUR LE PALIER:',iapp
        endif
!
!
100  end do
!
!     FIN DE LA BOUCLE SUR LES PALIERS
!
    call jedema()
!
end subroutine

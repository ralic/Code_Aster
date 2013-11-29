subroutine amumpc(action, kxmps, csolu, vcine, nbsol,&
                  iret, impr, ifmump, prepos, pcentp)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
!--------------------------------------------------------------
! OBJET: DRIVER EN MODE COMPLEXE DE LA RESOLUTION DE SYSTEMES LINEAIRES
!        VIA MUMPS (EN SIMPLE PRECISION POUR MUMPS UNIQUEMENT)
!
! IN : ACTION :
!     /'PRERES'  : POUR DEMANDER LA FACTORISATION
!     /'RESOUD'  : POUR DEMANDER LA DESCENTE/REMONTEE
!     /'DETR_MAT/OCC': POUR DEMANDER LA DESTRUCTION DE L'INSTANCE MUMPS
!                  ASSOCIEE A UNE MATRICE
!
! IN : KXMPS (I)   : INDICE DE L'INSTANCE MUMPS DANS CMPS
! VAR: CSOLU (C)   : EN ENTREE : VECTEUR SECOND MEMBRE (COMPLEXE)
!                    EN SORTIE : VECTEUR SOLUTION (COMPLEXE)
!            (SI ACTION=RESOUD)
! IN : VCINE (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
!            (SI ACTION=RESOUD)
! OUT : IRET (I) : CODE_RETOUR :
!            0 : OK
!            1 : ERREUR (DANS LE CAS OU MUMPS EST UTILISE EN PRE_COND)
!            2 : MATRICE NUMERIQUEMENT SINGULIERE
! IN  : NBSOL  : NBRE DE SYSTEMES A RESOUDRE
! IN  : IMPR,IFMUMP : PARAMETRES POUR SORTIE FICHIER MATRICE CF AMUMPH
! IN  : PREPOS (LOG) : SI .TRUE. ON FAIT LES PRE ET POSTTRAITEMENTS DE
!           MISE A L'ECHELLE DU RHS ET DE LA SOLUTION (MRCONL) ET DE LA
!           PRISE EN COMPTE DES AFFE_CHAR_CINE (CSMBGG).
!           SI .FALSE. ON NE LES FAIT PAS (PAR EXEMPLE EN MODAL).
! IN  : PCENTP  VECTEUR D'ENTIER GERE PAR AMUMPH POUR PARAMETRER LES
!                STRATEGIES D'ADAPTATION EN CAS DE PB PCENT_PIVOT
!---------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
!
#include "asterf.h"
#include "aster_types.h"
#include "asterc/matfpe.h"
#include "asterfort/amumpi.h"
#include "asterfort/amumpm.h"
#include "asterfort/amumpp.h"
#include "asterfort/amumpt.h"
#include "asterfort/amumpu.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "mumps/cmumps.h"
    character(len=*) :: action
    character(len=14) :: impr
    character(len=19) :: vcine, nosolv
    integer :: iret, nbsol, kxmps, ifmump, pcentp(2)
    complex(kind=8) :: csolu(*)
    logical :: prepos
!
#ifdef _HAVE_MUMPS
#include "aster_mumps.h"
#include "mpif.h"
#include "jeveux.h"
    type (cmumps_struc) , pointer :: cmpsk => null()
    integer :: jslvk, jslvr, rang, nbproc, niv, ifm, ibid, ietdeb, ifactm, nbfact
    integer :: ietrat, jrefa, nprec, jslvi, ifact, iaux, iaux1, vali(4), pcpi
    character(len=1) :: rouc, type, prec
    character(len=5) :: etam, klag2
    character(len=8) :: ktypr
    character(len=12) :: usersm, k12bid
    character(len=14) :: nonu
    character(len=19) :: nomat
    character(len=24) :: kmonit(12), k24aux, kvers, k24bid
    real(kind=8) :: epsmax, valr(2), rctdeb, rbid(1), temps(6), epsmat
    logical :: lquali, ldist, lresol, lmd, lbid, lpreco, lbis, lpb13, ldet
    logical :: lopfac
    call jemarq()
!
!       ------------------------------------------------
!        INITS
!       ------------------------------------------------
! --- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LA BIBLIOTHEQUE MKL
! --  CAR CES EXCEPTIONS NE SONT PAS JUSTIFIEES
    call matfpe(-1)
    call infniv(ifm, niv)
!
! --- PARAMETRE POUR IMPRESSION FICHIER
    lresol=((impr(1:3).eq.'NON').or.(impr(1:9).eq.'OUI_SOLVE'))
!
! --- TYPE DE SYSTEME: REEL OU COMPLEXE
    type='C'
    ASSERT(kxmps.gt.0)
    ASSERT(kxmps.le.nmxins)
    nomat=nomats(kxmps)
    nosolv=nosols(kxmps)
    nonu=nonus(kxmps)
    etam=etams(kxmps)
    rouc=roucs(kxmps)
    prec=precs(kxmps)
    ASSERT((rouc.eq.'C').and.(prec.eq.'S'))
    cmpsk=>cmps(kxmps)
    iret=0
    call jeveuo(nosolv//'.SLVK', 'E', jslvk)
    call jeveuo(nosolv//'.SLVR', 'L', jslvr)
    call jeveuo(nosolv//'.SLVI', 'E', jslvi)
!
! --- L'UTILISATEUR VEUT-IL UNE ESTIMATION DE LA QUALITE DE LA SOL ?
! --- => LQUALI
!
    epsmax=zr(jslvr-1+2)
    lquali=(epsmax.gt.0.d0)
!
! --- POUR "ELIMINER" LE 2EME LAGRANGE :
! --- OPTION DEBRANCHEE SI CALCUL DE DETERMINANT
    klag2=zk24(jslvk-1+6)
    lbis=klag2(1:5).eq.'LAGR2'
!
! --- TRES PROBABLEMENT COMMANDE FACTORISER (POSTTRAITEMENTS
! --- INITIALISE A 'XXXX'). ON NE DETRUIRA RIEN A L'ISSU DE LA
! --- FACTO, AU CAS OU UN OP. RESOUDRE + RESI_RELA>0 SUIVRAIT
    if (zk24(jslvk-1+11)(1:4) .eq. 'XXXX') then
        lopfac=.true.
    else
        lopfac=.false.
    endif
!
! --- TYPE DE RESOLUTION
    ktypr=zk24(jslvk-1+3)
    if (ktypr(1:6) .eq. 'SYMDEF') then
        call utmess('F', 'FACTOR_80')
    endif
!
! --- PARAMETRE NPREC
!
    nprec=zi(jslvi)
!
! --- MUMPS PARALLELE DISTRIBUE ?
    call jeveuo(nomat//'.REFA', 'L', jrefa)
    ldist=(zk24(jrefa-1+11).ne.'MPI_COMPLET')
    rang=cmpsk%myid
    nbproc=cmpsk%nprocs
!
! --- MATRICE ASTER DISTRIBUEE ?
    lmd = zk24(jslvk-1+10)(1:3).eq.'OUI'
!
! --- MUMPS EST-IL UTILISE COMME PRECONDITIONNEUR ?
! --- SI OUI, ON DEBRANCHE LES ALARMES ET INFO (PAS LES UTMESS_F)
    lpreco = zk24(jslvk-1+8)(1:3).eq.'OUI'
!
! --- FILTRAGE DE LA MATRICE DONNEE A MUMPS (UNIQUEMENT NON LINEAIRE)
    epsmat=zr(jslvr-1+1)
!
! --- STRATEGIE MEMOIRE POUR MUMPS
    usersm=zk24(jslvk-1+9)
    nbfact=zi(jslvi-1+6)
!
! --- POUR MONITORING
    call amumpt(0, kmonit, temps, rang, nbproc,&
                kxmps, lquali, type, ietdeb, ietrat,&
                rctdeb, ldist)
!
!     ------------------------------------------------
!     ------------------------------------------------
    if (action(1:6) .eq. 'PRERES') then
!     ------------------------------------------------
!     ------------------------------------------------
!
!       ------------------------------------------------
!        INITIALISATION DE L'OCCURENCE MUMPS KXMPS:
!       ------------------------------------------------
        call amumpi(0, lquali, ldist, kxmps, type)
        call cmumps(cmpsk)
        rang=cmpsk%myid
        nbproc=cmpsk%nprocs
!
!       --------------------------------------------------------------
!        CHOIX ICNTL VECTEUR DE PARAMETRES POUR MUMPS (ANALYSE+FACTO):
!       --------------------------------------------------------------
        call amumpi(2, lquali, ldist, kxmps, type)
!
!       ----------------------------------------------------------
!        ON RECUPERE ET STOCKE DS SD_SOLVEUR LE NUMERO DE VERSION
!        LICITE
!       ----------------------------------------------------------
        call amumpu(3, type, kxmps, k12bid, ibid,&
                    lbid, kvers, ibid)
        zk24(jslvk-1+12)=kvers
!
!       -----------------------------------------------------
!       CALCUL DU DETERMINANT PART I ?
!       -----------------------------------------------------
        ldet=.false.
        if (zi(jslvi-1+5) .eq. 1) then
            select case (kvers)
                case('4.10.0')
! --- ON DEBRANCHE ELIM_LAGR='LAGR2' CAR CELA FAUSSE LA VALEUR DU DETER
! --- MINANT PAR RAPPORT AUX AUTRES SOLVEURS DIRECTS
                if ((niv.ge.2) .and. (lbis) .and. (.not.lpreco)) then
                    call utmess('I', 'FACTOR_88')
                endif
                zk24(jslvk-1+6)='NON'
                klag2='NON'
                lbis=.false.
                ldet=.true.
                case('4.9.2')
                call utmess('F', 'FACTOR_87', sk=kvers)
            end select
        endif
!
!       ------------------------------------------------
!        REMPLISSAGE DE LA MATRICE MUMPS :
!       ------------------------------------------------
        call amumpt(1, kmonit, temps, rang, nbproc,&
                    kxmps, lquali, type, ietdeb, ietrat,&
                    rctdeb, ldist)
        call amumpm(ldist, kxmps, kmonit, impr, ifmump,&
                    klag2, type, lmd, epsmat, ktypr,&
                    lpreco)
!
!       -----------------------------------------------------
!       CONSERVE-T-ON LES FACTEURS OU NON ?
!       -----------------------------------------------------
        if (zi(jslvi-1+4) .eq. 1) then
            select case (kvers)
                case('4.10.0')
                cmpsk%icntl(31)=1
                case('4.9.2')
                if ((niv.ge.2) .and. (.not.lpreco)) then
                    call utmess('I', 'FACTOR_86', sk=kvers)
                endif
            end select
        endif
!
!       ------------------------------------------------
!        ANALYSE MUMPS:
!       ------------------------------------------------
!       INITIALISATIONS POUR ANALYSE+FACTO+CORRECTION EVENTUELLE
        ifact=0
        lpb13=.false.
        if (usersm(1:4) .eq. 'AUTO') then
            ifactm=pcentp(1)
        else
            ifactm=1
        endif
!
 10     continue
        call amumpt(2, kmonit, temps, rang, nbproc,&
                    kxmps, lquali, type, ietdeb, ietrat,&
                    rctdeb, ldist)
        cmpsk%job = 1
        call cmumps(cmpsk)
        call amumpt(4, kmonit, temps, rang, nbproc,&
                    kxmps, lquali, type, ietdeb, ietrat,&
                    rctdeb, ldist)
!
!       ------------------------------------------------
!        GESTION ERREURS ET MENAGE ASTER:
!       ------------------------------------------------
        if (cmpsk%infog(1) .eq. 0) then
!              -- C'EST OK
            else if ((cmpsk%infog(1).eq.-5).or.(cmpsk%infog(1).eq.-7))&
        then
            call utmess('F', 'FACTOR_64')
        else if (cmpsk%infog(1).eq.-6) then
            iret=2
            goto 99
        else
            iaux=cmpsk%infog(1)
            if (iaux .lt. 0) then
                call utmess('F', 'FACTOR_55', si=iaux)
            else
                if (.not.lpreco) then
                    call utmess('A', 'FACTOR_55', si=iaux)
                endif
            endif
        endif
        if (zk24(jslvk-1+4) .ne. 'AUTO' .and. cmpsk%icntl(7) .ne. cmpsk% infog(7) .and.&
            (.not.lpreco)) then
            call utmess('A', 'FACTOR_50', sk=zk24(jslvk-1+4))
        endif
!
!       -----------------------------------------------------
!        CHOIX DE LA STRATEGIE MUMPS POUR LA GESTION MEMOIRE
!       -----------------------------------------------------
        if (.not.lpb13) call amumpu(1, 'C', kxmps, usersm, ibid,&
                                    lbid, k24bid, nbfact)
!
! ---   ON SORT POUR REVENIR A AMUMPH ET DETRUIRE L'OCCURENCE MUMPS
! ---   ASSOCIEE
        if (usersm(1:4) .eq. 'EVAL') goto 99
!
!       -----------------------------------------------------
!       CALCUL DU DETERMINANT PART II ?
!       -----------------------------------------------------
        if (ldet) cmpsk%icntl(33)=1
!
!       ------------------------------------------------
!        FACTORISATION NUMERIQUE MUMPS:
!       ------------------------------------------------
!
! --- SI GESTION_MEMOIRE='AUTO'
! --- ON TENTE PLUSIEURS (PCENTP(1)) FACTORISATIONS NUMERIQUES EN
! --- MULTIPLIANT, A CHAQUE ECHEC, L'ANCIEN PCENT_PIVOT PAR PCENTP(2)
! --- VOIRE EN PASSANT EN OOC (EN DERNIER RESSORT).
! --- AUTO-ADAPTATION DU PARAMETRAGE SOLVEUR/PCENT_PIVOT:
! --- ON MODIFIE LE PARAMETRE DANS LA SD_SOLVEUR A LA VOLEE POUR NE
! --- PAS PERDRE DE TEMPS LA PROCHAINE FOIS. CETTE VALEUR N'EST VALABLE
! --- QUE DANS L'OPERATEUR CONSIDERE.
! --- ON FAIT LA MEME CHOSE EN CAS DE PB D'ALLOCATION MEMOIRE (INFOG=-13
! --- CELA PEUT ETRE DU A UN ICNTL(23) MAL ESTIME
!
        cmpsk%job = 2
        if (lresol) then
            pcpi=cmpsk%icntl(14)
            do ifact = 1, ifactm
                call cmumps(cmpsk)
                iaux=cmpsk%infog(1)
                iaux1=cmpsk%icntl(23)
!
! --- TRAITEMENT CORRECTIF ICNTL(14)
                if ((iaux.eq.-8) .or. ((iaux.eq.-9).and.(iaux1.eq.0)) .or. (iaux.eq.-14)&
                    .or. (iaux.eq.-15) .or. (iaux.eq.-17) .or. (iaux.eq.-20)) then
                    if (ifact .eq. ifactm) then
! ---  ICNTL(14): PLUS DE NOUVELLE TENTATIVE POSSIBLE
                        if (lpreco) then
!                 -- MUMPS EST APPELE COMME PRECONDITIONNEUR
!                 -- ON SORT AVEC UN CODE RETOUR NON NUL
                            iret = 1
                            goto 99
                        else
                            vali(1)=ifactm
                            vali(2)=pcpi
                            vali(3)=cmpsk%icntl(14)
                            call utmess('F', 'FACTOR_53', ni=3, vali=vali)
                        endif
                    else
! ---  ICNTL(14): ON MODIFIE DES PARAMETRES POUR LA NOUVELLE TENTATIVE ET ON REVIENT A L'ANALYSE
                        cmpsk%icntl(14)=cmpsk%icntl(14) * to_mumps_int(pcentp(2))
                        zi(jslvi-1+2)=cmpsk%icntl(14)
                        if (niv .ge. 2) then
                            vali(1)=cmpsk%icntl(14)/pcentp(2)
                            vali(2)=cmpsk%icntl(14)
                            vali(3)=ifact
                            vali(4)=ifactm
                            if (.not.lpreco) then
                                call utmess('I', 'FACTOR_58', ni=4, vali=vali)
                            endif
                        endif
                        ifactm=max(ifactm-ifact,1)
                        goto 10
                    endif
!
! --- TRAITEMENT CORRECTIF ICNTL(23)
! --- CE N'EST UTILE QU' UNE FOIS D'OU LE CONTROLE DE LPB13
                else if (((iaux.eq.-13).or.((iaux.eq.-9).and.(iaux1.ne.0))).and.(.not.lpb13)) then
! ---  ICNTL(23): ON MODIFIE DES PARAMETRES POUR LA NOUVELLE TENTATIVE ET ON REVIENT A L'ANALYSE
                    if (niv .ge. 2) then
                        vali(1)=cmpsk%icntl(23)
                        if (.not.lpreco) then
                            call utmess('I', 'FACTOR_85', si=vali(1))
                        endif
                    endif
                    lpb13=.true.
                    cmpsk%icntl(23)=0
                    cmpsk%icntl(22)=1
                    ifactm=max(ifactm-ifact,1)
                    goto 10
                else
! ---  SORTIE STANDARD SANS ERREUR
                    exit
                endif
            enddo
        endif
!
! ---  AFFICHAGE DE CONTROLE
        if (niv .ge. 2) then
            write(ifm,*)
            write(ifm,*)&
     &      '<AMUMPC> FACTO. NUM. - NBRE TENTATIVES/MAX: ',ifact,ifactm
        endif
        call amumpt(6, kmonit, temps, rang, nbproc,&
                    kxmps, lquali, type, ietdeb, ietrat,&
                    rctdeb, ldist)
!
!       ------------------------------------------------
!        GESTION ERREURS ET MENAGE ASTER (SAUF ERREUR ICNTL(14/23)
!           TRAITEE EN AMONT):
!       ------------------------------------------------
        valr(1)=(cmpsk%infog(13)*100.d0)/cmpsk%n
        valr(2)=cmpsk%icntl(14)*1.d0
        if (valr(1) .ge. valr(2)) then
            if ((niv.ge.2) .and. (.not.lpreco)) then
                call utmess('I', 'FACTOR_73', nr=2, valr=valr)
            endif
        endif
        if (cmpsk%infog(1) .eq. 0) then
!              -- C'EST OK
        else if (cmpsk%infog(1).eq.-10) then
            iret=2
            goto 99
        else if (cmpsk%infog(1).eq.-13) then
            call utmess('F', 'FACTOR_54')
        else if (cmpsk%infog(1).eq.-37) then
            call utmess('F', 'FACTOR_65')
        else if (cmpsk%infog(1).eq.-90) then
            call utmess('F', 'FACTOR_66')
        else
            iaux=cmpsk%infog(1)
            if (iaux .lt. 0) then
                call utmess('F', 'FACTOR_55', si=iaux)
            else
                if (.not.lpreco) then
                    call utmess('A', 'FACTOR_55', si=iaux)
                endif
            endif
        endif
!
!       ------------------------------------------------
!        DETECTION DE SINGULARITE SI NECESSAIRE:
!       ------------------------------------------------
        call amumpu(2, 'C', kxmps, k12bid, nprec,&
                    lresol, k24bid, ibid)
!
!       ------------------------------------------------
!        RECUPERATION DU DETERMINANT SI NECESSAIRE:
!       ------------------------------------------------
        call amumpu(4, 'C', kxmps, k12bid, ibid,&
                    lbid, k24bid, ibid)
!
!       ON SOULAGE LA MEMOIRE JEVEUX DES QUE POSSIBLE D'OBJETS MUMPS
!       INUTILES
        if ((( rang.eq.0).and.(.not.ldist)) .or. (ldist)) then
            if (.not.lquali .and. .not.lopfac) then
                if (ldist) then
                    deallocate(cmpsk%a_loc,stat=ibid)
                    deallocate(cmpsk%irn_loc,stat=ibid)
                    deallocate(cmpsk%jcn_loc,stat=ibid)
                else
                    deallocate(cmpsk%a,stat=ibid)
                    deallocate(cmpsk%irn,stat=ibid)
                    deallocate(cmpsk%jcn,stat=ibid)
                endif
            endif
        endif
!
!     ------------------------------------------------
!     ------------------------------------------------
    else if (action(1:6).eq.'RESOUD') then
!     ------------------------------------------------
!     ------------------------------------------------
!
!       ------------------------------------------------
!        PRETRAITEMENTS ASTER DU/DES SECOND(S) MEMBRE(S) :
!       ------------------------------------------------
        call amumpt(7, kmonit, temps, rang, nbproc,&
                    kxmps, lquali, type, ietdeb, ietrat,&
                    rctdeb, ldist)
        call amumpp(0, nbsol, kxmps, ldist, type,&
                    impr, ifmump, lbis, rbid, csolu,&
                    vcine, prepos, lpreco)
!
!       --------------------------------------------------------------
!        CHOIX ICNTL VECTEUR DE PARAMETRES POUR MUMPS (SOLVE):
!       --------------------------------------------------------------
        call amumpi(3, lquali, ldist, kxmps, type)
!
!       ------------------------------------------------
!        RESOLUTION MUMPS :
!       ------------------------------------------------
        call amumpt(8, kmonit, temps, rang, nbproc,&
                    kxmps, lquali, type, ietdeb, ietrat,&
                    rctdeb, ldist)
        cmpsk%job = 3
        if (lresol) call cmumps(cmpsk)
        call amumpt(10, kmonit, temps, rang, nbproc,&
                    kxmps, lquali, type, ietdeb, ietrat,&
                    rctdeb, ldist)
!
!       ------------------------------------------------
!        GESTION ERREURS ET MENAGE ASTER:
!       ------------------------------------------------
        if (cmpsk%infog(1) .eq. 0) then
!              -- C'EST OK
        else if ((cmpsk%infog(1).eq.8).and.(lquali)) then
            iaux=cmpsk%icntl(10)
            if (.not.lpreco) then
                call utmess('A', 'FACTOR_62', si=iaux)
            endif
        else if (cmpsk%infog(1).lt.0) then
            iaux=cmpsk%infog(1)
            call utmess('F', 'FACTOR_55', si=iaux)
        else if (cmpsk%infog(1).eq.4) then
!          -- PERMUTATION DE COLONNES, CMPSK%JCN MODIFIE VOLONTAIREMENT
!          -- PAR MUMPS. IL NE FAUT DONC PAS LE MANIPULER TEL QUE
!          -- PAS GRAVE POUR ASTER.
        else
            iaux=cmpsk%infog(1)
            if (.not.lpreco) then
                call utmess('A', 'FACTOR_55', si=iaux)
            endif
        endif
! --- CONTROLE DE L'ERREUR SUR LA SOLUTION :
        if (lquali) then
            if (cmpsk%rinfog(9) .gt. epsmax) then
                valr(1)=cmpsk%rinfog(9)
                valr(2)=epsmax
                call utmess('F', 'FACTOR_57', nr=2, valr=valr)
            endif
        endif
!
!       ------------------------------------------------
!        POST-TRAITEMENTS ASTER DE/DES (LA) SOLUTION(S) :
!       ------------------------------------------------
        call amumpp(2, nbsol, kxmps, ldist, type,&
                    impr, ifmump, lbis, rbid, csolu,&
                    vcine, prepos, lpreco)
!
!       ------------------------------------------------
!        AFFICHAGE DU MONITORING :
!       ------------------------------------------------
        call amumpt(12, kmonit, temps, rang, nbproc,&
                    kxmps, lquali, type, ietdeb, ietrat,&
                    rctdeb, ldist)
!
!     ------------------------------------------------
!     ------------------------------------------------
    else if (action(1:5).eq.'DETR_') then
!     ------------------------------------------------
!     ------------------------------------------------
!
!
!       ------------------------------------------------
!        MENAGE ASTER ET MUMPS:
!       ------------------------------------------------
        if (nomats(kxmps) .ne. ' ') then
            if ((( rang.eq.0).and.(.not.ldist)) .or. (ldist)) then
                if (ldist) then
                    deallocate(cmpsk%a_loc,stat=ibid)
                    deallocate(cmpsk%irn_loc,stat=ibid)
                    deallocate(cmpsk%jcn_loc,stat=ibid)
                else
                    deallocate(cmpsk%a,stat=ibid)
                    deallocate(cmpsk%irn,stat=ibid)
                    deallocate(cmpsk%jcn,stat=ibid)
                endif
            endif
            etams(kxmps)=' '
            nonus(kxmps)=' '
            nomats(kxmps)=' '
            nosols(kxmps)=' '
            roucs(kxmps)=' '
            precs(kxmps)=' '
            cmpsk%job = -2
            call cmumps(cmpsk)
! NETTOYAGE OBJETS AUXILIAIRES AU CAS OU
            k24aux='&&TAILLE_OBJ_MUMPS'
            call jeexin(k24aux, ibid)
            if (ibid .ne. 0) call jedetr(k24aux)
!
            k24aux='&&AMUMP.PIVNUL'
            call jeexin(k24aux, ibid)
            if (ibid .ne. 0) call jedetr(k24aux)
!
            k24aux='&&AMUMP.DETERMINANT'
            call jeexin(k24aux, ibid)
            if (ibid .ne. 0) call jedetr(k24aux)
        endif
    endif
!
!     -- ON REACTIVE LA LEVEE D'EXCEPTION
    99 call matfpe(1)
    call jedema()
!
#endif
end subroutine

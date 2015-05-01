subroutine algocg(sdstat, defico, resoco, solveu, matass,&
                  ctccvg)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisr.h"
#include "asterfort/cfecrd.h"
#include "asterfort/cfgcac.h"
#include "asterfort/cfgccj.h"
#include "asterfort/cfgcin.h"
#include "asterfort/cfgcpc.h"
#include "asterfort/cfgcpr.h"
#include "asterfort/cfgcrl.h"
#include "asterfort/cfgcsg.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedupo.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmrvai.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    character(len=24) :: sdstat
    character(len=24) :: defico, resoco
    character(len=19) :: matass, solveu
    integer :: ctccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! ALGO. POUR CONTACT    : GRADIENT CONJUGUE PROJETE
! ALGO. POUR FROTTEMENT : SANS
!
! ----------------------------------------------------------------------
!
!
!
! RESOLUTION DE : C.DU + AT.MU  = F
!                 A(U+DU)      <= E (= POUR LES LIAISONS ACTIVES)
!
! AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
!
!      A = MATRICE DE CONTACT
!
!      C = ( K  BT ) MATRICE DE RIGIDITE INCLUANT LES LAGRANGE
!          ( B  0  )
!
!      U = ( DEPL )
!          ( LAM  )
!
!      F = ( DL  ) DANS LA PHASE DE PREDICTION
!          ( DUD )
!
!      F = ( L - QT.SIG - BT.LAM  ) AU COURS D'UNE ITERATION DE NEWTON
!          (           0          )
!
! IN  SDSTAT : SD STATISTIQUES
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! OUT CTCCVG : CODE RETOUR CONTACT DISCRET
!                -1 : PAS DE CALCUL DU CONTACT DISCRET
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXI D'ITERATIONS
!                 2 : MATRICE SINGULIERE
!
!
!
!
    integer :: ifm, niv
    aster_logical :: conjug
    integer :: iliai, iter, premax
    integer :: neq, nbliac, nbliai
    integer :: gcpmax
    character(len=16) :: precon, search, pceffe
    character(len=19) :: sgradm, sgradp, sgrprm, sgrprp, mum
    integer :: jsgram, jsgrap, jsgprm, jsgprp, jmum
    character(len=19) :: mu
    integer :: jmu
    character(len=19) :: ddeplc, ddelt
    real(kind=8) :: tole, coefrs
    real(kind=8) :: ninf, ninfpc, alpha, epsi
    real(kind=8), pointer :: vddelt(:) => null()
    real(kind=8), pointer :: ddepc(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write(ifm,*) '<CONTACT><CALC> ALGO_CONTACT   : GRADIENT '//&
     &    'CONJUGUE PROJETE'
        write(ifm,*) '<CONTACT><CALC> ALGO_FROTTEMENT: SANS'
    endif
!
! --- INITIALISATION DES VARIABLES
!
    nbliai = cfdisd(resoco,'NBLIAI')
    neq = cfdisd(resoco,'NEQ' )
    ctccvg = 0
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    mu = resoco(1:14)//'.MU'
    sgradm = resoco(1:14)//'.SGDM'
    sgradp = resoco(1:14)//'.SGDP'
    sgrprm = resoco(1:14)//'.SGPM'
    sgrprp = resoco(1:14)//'.SGPP'
    mum = resoco(1:14)//'.MUM'
    call jeveuo(mu, 'E', jmu)
    call jeveuo(sgradm, 'E', jsgram)
    call jeveuo(sgradp, 'E', jsgrap)
    call jeveuo(sgrprm, 'E', jsgprm)
    call jeveuo(sgrprp, 'E', jsgprp)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPLC: INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
! --- DDELT : INCREMENT DE SOLUTION ITERATION DE CONTACT
!
    ddeplc = resoco(1:14)//'.DELC'
    ddelt = resoco(1:14)//'.DDEL'
!
! --- INITIALISATION DES VECTEURS DE TRAVAIL
!
    call jedupo(mu, 'V', mum, .false._1)
    call jeveuo(mum, 'E', jmum)
!
! ======================================================================
!                             INITIALISATIONS
! ======================================================================
!
    iter = 1
    conjug = .false.
    tole = r8prem()
    ninfpc = 0.d0
!
! --- RECUPERATION DU CRITERE DE CONVERGENCE
!
    epsi = cfdisr(defico,'RESI_ABSO')
    coefrs = cfdisr(defico,'COEF_RESI')
    gcpmax = 10*nbliai
    premax = cfdisi(defico,'ITER_PRE_MAXI')
    if (cfdisi(defico,'ITER_GCP_MAXI') .ne. 0) then
        gcpmax = max(gcpmax,cfdisi(defico,'ITER_GCP_MAXI'))
    endif
    if (cfdisi(defico,'PRE_COND') .eq. 1) then
        precon = 'DIRICHLET'
    else
        precon = 'SANS'
    endif
    if (cfdisi(defico,'RECH_LINEAIRE') .eq. 1) then
        search = 'NON_ADMISSIBLE'
    else
        search = 'ADMISSIBLE'
    endif
!
    if (niv .ge. 2) then
        write (ifm,9010) gcpmax
    endif
!
! --- INITIALISATION A PARTIR DU CHAMP DE MULTIPLICATEURS INITIAL
!
    call cfgcin(resoco, matass, solveu, neq, nbliai)
!
! ======================================================================
!                    REPRISE DE LA BOUCLE PRINCIPALE
! ======================================================================
!
 40 continue
!
    if (niv .eq. 2) then
        write (ifm,*) '<CONTACT><CALC> --------------------------------'
        write (ifm,*) '<CONTACT><CALC> ITERATION DE GCP = ',iter
    endif
!
! --- CALCUL DU SOUS-GRADIENT
!
    call cfgcsg(resoco, neq, nbliai, tole, ninf)
!
! --- A-T-ON CONVERGE ?
!
    if (niv .eq. 2) then
        write (ifm,9060) ninf,epsi
    endif
!
    if (ninf .lt. epsi) then
        goto 160
    endif
!
! --- PRECONDITIONNEMENT UNIQUEMENT AU VOISINAGE DE LA SOLUTION
! --- LE VOISINAGE EST DEFINI PAR COEF_RESI
!
    if (iter .eq. 1) then
        ninfpc = coefrs*ninf
    endif
    pceffe = precon
    if (ninfpc .gt. 0.d0) then
        if (ninf .gt. ninfpc) then
            pceffe = 'SANS'
        endif
    endif
!
! --- PRECONDITIONNEMENT
!
    call cfgcpc(resoco, matass, solveu, neq, nbliai,&
                pceffe, tole, premax, epsi)
!
! --- CONJUGAISON
!
    call cfgccj(resoco, nbliai, conjug)
!
! --- RECHERCHE LINEAIRE: PAS D'AVANCEMENT
!
    call cfgcrl(resoco, neq, nbliai, matass, solveu,&
                alpha)
!
! --- PROJECTION DU PAS D'AVANCEMENT
!
    call cfgcpr(resoco, matass, solveu, neq, nbliai,&
                search, alpha)
!
! --- ACTUALISATION DE {DDEPLC} = {DDEPLC} - ALPHA . {DDELT}
!
    call jeveuo(ddelt(1:19) //'.VALE', 'L', vr=vddelt)
    call jeveuo(ddeplc(1:19)//'.VALE', 'E', vr=ddepc)
    call daxpy(neq, -alpha, vddelt, 1, ddepc,&
               1)
!
! --- ON VERIFIE SI L'ETAT DE CONTACT A CHANGE (ON NE CONJUGUE PAS)
!
    conjug = .true.
    do 90 iliai = 1, nbliai
        if (((zr(jmum-1+iliai).le.tole).and. (zr(jmu -1+iliai) .gt.tole)) .or.&
            ((zr(jmum-1+iliai).gt.tole).and. (zr(jmu -1+ iliai).le.tole))) then
            conjug = .false.
            if (niv .eq. 2) then
                write (ifm,*) '<CONTACT><CALC>'//&
     &        ' CHANGEMENT DE L''ETAT DE CONTACT'
            endif
            goto 100
        endif
 90 end do
100 continue
!
! --- MISE A JOUR DES GRADIENTS ET DES DIRECTIONS DE RECHERCHE
!
    call dcopy(nbliai, zr(jsgrap), 1, zr(jsgram), 1)
    call dcopy(nbliai, zr(jsgprp), 1, zr(jsgprm), 1)
    call dcopy(nbliai, zr(jmu), 1, zr(jmum), 1)
!
! --- ON PASSE A L'ITERATION SUIVANTE
!
    iter = iter + 1
!
! --- A-T-ON DEPASSE LE NOMBRE D'ITERATIONS DE CONTACT AUTORISE ?
!
    if (iter .ge. gcpmax) then
        ctccvg = 1
        goto 160
    endif
!
    goto 40
!
! ======================================================================
!                            ON A CONVERGE
! ======================================================================
!
160 continue
!
! --- ACTIVATION DES LIAISONS ET CALCUL DE LA FORCE DE CONTACT
!
    call cfgcac(resoco, tole, neq, nbliai, nbliac)
!
! --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
!
    call cfecrd(resoco, 'NBLIAC', nbliac)
!
    if (niv .ge. 2) then
        write(ifm,9020) iter
    endif
!
! --- SAUVEGARDE DES INFOS DE DIAGNOSTIC
!
    call nmrvai(sdstat, 'CTCD_ALGO_ITER', 'E', iter)
    call nmrvai(sdstat, 'CONT_NBLIAC', 'E', nbliac)
!
    call jedema()
!
    9010 format (' <CONTACT><CALC> DEBUT DES ITERATIONS (MAX: ',i8,')')
    9020 format (' <CONTACT><CALC> FIN DES ITERATIONS (NBR: ',i8,')')
!
!
    9060 format (' <CONTACT><CALC> NORME INFINIE DU RESIDU : ',&
     &        1pe12.5,' (CRITERE: ',1pe12.5,')')
end subroutine

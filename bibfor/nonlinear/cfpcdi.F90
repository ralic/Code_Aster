subroutine cfpcdi(resoco, neq, nbliai, tole, epsipc,&
                  mu, apcoef, apddl, appoin, inliac,&
                  matass, solveu, premax, ssgrad, ssgrpr)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/caladu.h"
#include "asterfort/calatm.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/r8inir.h"
#include "asterfort/resoud.h"
#include "asterfort/utmess.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
    character(len=24) :: resoco
    integer :: neq, nbliai, apddl(*), appoin(*), inliac(*), premax
    real(kind=8) :: apcoef(*), ssgrad(*), ssgrpr(*), mu(*)
    real(kind=8) :: tole, epsipc
    character(len=19) :: matass, solveu
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (RESOLUTION - GCP)
!
! PRECONDITIONNEMENT DE L'ALGORITHME DU GRADIENT CONJUGUE PROJETE
!
! ----------------------------------------------------------------------
!
! RESOLUTION D'UN PROBLEME ANNEXE A DEPLACEMENT IMPOSE SUR LES NOEUDS
! EFFECTIVEMENT EN CONTACT.
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS DU SYSTEME
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! IN  TOLE   : TOLERANCE DE DETECTION DE MU NUL
! IN  EPSIPC : TOLERANCE SOLVEUR ITERATIF
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  PREMAX : NOMBRE MAXI D'ITERATIONS DU SOLVEUR ITERATIF
! IN  SOLVEU : SD SOLVEUR
! IN  SSGRAD : SOUS-GRADIENT NON-PRECONDITIONNE
! OUT SSGRPR : SOUS-GRADIENT PRECONDITIONNE
!
!
!
!
    integer :: ifm, niv
    real(kind=8) :: numer, denom, conver, alpha
    real(kind=8) :: numerp, numerm, beta
    real(kind=8) :: convm, coef, r8bid
    integer :: iliac, iliai, jdecal, nbddl, iterat, nbliac
    character(len=24) :: cncin0, secmbr, ddelt, pcresi, pcdire, pcdepl
    integer :: jsecmb, jddelt, jpcres, jpcdir, jpcdep
    character(len=19) :: k19bla
    complex(kind=8) :: c16bid
    parameter    (coef=1.d-2)
    integer :: iret
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    k19bla = ' '
!
! --- ACCES AUX CHAMPS DE TRAVAIL
!
    cncin0 = resoco(1:14)//'.CIN0'
    secmbr = resoco(1:14)//'.SECM'
    ddelt = resoco(1:14)//'.DDEL'
    call jeveuo(secmbr(1:19)//'.VALE', 'E', jsecmb)
    call jeveuo(ddelt(1:19)//'.VALE', 'E', jddelt)
    pcresi = resoco(1:14)//'.PCRS'
    pcdire = resoco(1:14)//'.PCDR'
    pcdepl = resoco(1:14)//'.PCUU'
    call jeveuo(pcresi, 'E', jpcres)
    call jeveuo(pcdire, 'E', jpcdir)
    call jeveuo(pcdepl, 'E', jpcdep)
!
! --- INITIALISATION DE DELTA
!
    call r8inir(neq, 0.d0, zr(jddelt), 1)
!
! --- COMPTAGE DU NOMBRE DE LIAISONS REELLEMENT ACTIVES
!
    nbliac = 0
    do 10 iliai = 1, nbliai
        if ((mu(iliai).gt.tole) .or. (ssgrad(iliai).gt.epsipc)) then
            nbliac = nbliac + 1
            inliac(nbliac) = iliai
        endif
10  end do
!
! --- SI AUCUNE LIAISON ACTIVE ON SORT CAR
! --- LE PRECONDITIONNEUR EST INUTILE
!
    if (nbliac .eq. 0) then
        call dcopy(nbliai, ssgrad, 1, ssgrpr, 1)
        if (niv .ge. 2) then
            write (ifm,*) '<CONTACT><CALC> PAS DE '//&
     &                  'PRECONDITIONNEMENT (PAS DE LIAISONS ACTIVES)'
        endif
        goto 120
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT><CALC> PRECONDITIONNEUR DIRICHLET'
        write (ifm,9010) nbliac,epsipc
    endif
!
! --- NOMBRE D'ITERATIONS MAX
!
    if (premax .eq. 0) premax = 2*nbliac
!
! --- MISE A ZERO DES VECTEURS DE TRAVAIL
!
    call r8inir(neq, 0.d0, zr(jpcdep), 1)
    call r8inir(nbliai, 0.d0, zr(jpcres), 1)
    iterat = 1
!
! ======================================================================
! =========================== BOUCLE PRINCIPALE ========================
! ======================================================================
!
20  continue
!
! --- NOUVELLE VALEUR DU GRADIENT
!
    do 30 iliac = 1, nbliac
        iliai = inliac(iliac)
        jdecal = appoin(iliai)
        nbddl = appoin(iliai+1) - appoin(iliai)
!       RESIDU=A.UU-SSGRAD(ACT)
        call caladu(neq, nbddl, apcoef(1+jdecal), apddl(1+jdecal), zr(jpcdep),&
                    zr(jpcres-1+iliac))
        zr(jpcres-1+iliac) = zr(jpcres-1+iliac) - ssgrad(iliai)
30  end do
!
! --- TEST DE CONVERGENCE
!
    conver = -1.d0
    do 40 iliac = 1, nbliac
        conver = max(conver,abs(zr(jpcres-1+iliac)))
40  end do
    if (niv .ge. 2) then
        if (iterat .eq. 1) convm=10*conver/coef
        if (conver .lt. (coef*convm)) then
            write (ifm,9000) iterat,conver
            convm=conver
        endif
    endif
!
! --- ON A CONVERGE
!
    if (conver .lt. epsipc) then
        if (niv .ge. 2) then
            write (ifm,9020) iterat,conver
        endif
        goto 90
    endif
!
! --- NOUVELLE DIRECTION DE RECHERCHE
! --- DIRECH=RESIDU+BETA*DIRECH
!
    if (iterat .eq. 1) then
        numerp = ddot(nbliac,zr(jpcres),1,zr(jpcres),1)
        call dcopy(nbliac, zr(jpcres), 1, zr(jpcdir), 1)
    else
        numerm = numerp
        numerp = ddot(nbliac,zr(jpcres),1,zr(jpcres),1)
        beta = numerp/numerm
        call dscal(nbliac, beta, zr(jpcdir), 1)
        call daxpy(nbliac, 1.d0, zr(jpcres), 1, zr(jpcdir),&
                   1)
    endif
!
! --- CALCUL DU SECOND MEMBRE
! --- AT.DIRECH
!
    call r8inir(neq, 0.d0, zr(jsecmb), 1)
    do 50 iliac = 1, nbliac
        iliai = inliac(iliac)
        jdecal = appoin(iliai)
        nbddl = appoin(iliai+1) - appoin(iliai)
        call calatm(neq, nbddl, zr(jpcdir-1+iliac), apcoef(1+jdecal), apddl(1+jdecal),&
                    zr(jsecmb))
50  end do
!
! --- RESOLUTION
! --- DU=K-1*(AT.DIRECH)
!
    call resoud(matass, k19bla, solveu, cncin0, 0,&
                secmbr, ddelt, 'V', [0.d0], [c16bid],&
                k19bla, .true., 0, iret)
    call jeveuo(ddelt(1:19)//'.VALE', 'E', jddelt)
!
! --- PAS D'AVANCEMENT
!
    numer = ddot(nbliac,zr(jpcres),1,zr(jpcres),1)
    denom = ddot(neq,zr(jddelt),1,zr(jsecmb),1)
    alpha = numer/denom
!
    if (alpha .lt. 0.d0) then
        call utmess('F', 'CONTACT_7')
    endif
!
! --- ACTUALISATION DU SOUS GRADIENT ET DU DEPLACEMENT
!
    do 70 iliac = 1, nbliac
        iliai = inliac(iliac)
        ssgrpr(iliai) = ssgrpr(iliai) + alpha*zr(jpcdir-1+iliac)
70  end do
!
    call daxpy(neq, -alpha, zr(jddelt), 1, zr(jpcdep),&
               1)
!
!
! --- ON A ATTEINT LE NOMBRE D'ITERATION MAXIMAL
    if (iterat .ge. premax) goto 80
!
!
! --- ON N A PAS CONVERGE MAIS IL RESTE DES ITERATIONS A FAIRE
    iterat = iterat + 1
    goto 20
!
80  continue
!
!     ON A DEPASSE LE NOMBRE D'ITERATIONS MAX
    if (niv .ge. 2) then
        write (ifm,9000) iterat,conver
        call utmess('I', 'CONTACT_3', si=premax)
    endif
!
!
90  continue
!
! ======================================================================
! ============================= ON A CONVERGE ==========================
! ======================================================================
!
!     LES CRITERES DE CONVERGENCE SONT DECALES ENTRE L'APPELANT
!     ET CETTE ROUTINE. DU COUP, ON PEUT ENTRER ICI ET S'APERCEVOIR
!     QUE L'ON A RIEN A FAIRE. DANS CE CAS, ON RECOPIE.
    if (iterat .eq. 1) then
        do 100 iliac = 1, nbliac
            iliai = inliac(iliac)
            ssgrpr(iliai) = zr(jpcres-1+iliac)
100      continue
    endif
!
!     ON REPROJETE LE SOUS-GRADIENT PRECONDITIONNE POUR
!     ASSURER LA POSITIVITE DES MULTIPLICATEURS
    call dscal(nbliai, -1.d0, ssgrpr, 1)
    do 110 iliai = 1, nbliai
        if (mu(iliai) .le. tole) then
            ssgrpr(iliai) = max(ssgrpr(iliai),0.d0)
        endif
110  end do
!
!
120  continue
!
    call jedema()
!
    9000 format (' <CONTACT><CALC> PRECONDITIONNEUR : ITERATION =',i6,&
     &        ' RESIDU =',1pe12.5)
    9010 format (' <CONTACT><CALC> PRECONDITIONNEUR : ',i6,&
     &        ' LIAISON ACTIVES, CRITERE DE CONVERGENCE =',1pe12.5)
    9020 format (' <CONTACT><CALC> PRECONDITIONNEUR : ITERATION =',i6,&
     &        ' RESIDU =',1pe12.5,' => CONVERGENCE')
end subroutine

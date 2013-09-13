subroutine mpinv2(nbmesu, nbmode, nbabs, phi, rmesu,&
                  coef, xabs, lfonct, reta, retap,&
                  reta2p)
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
!     PROJ_MESU_MODAL : RESOLUTION DU SYSTEME PAR SVD OU PAR LU
!                       (DONNEES TEMPORELLES)
!
!     IN  : NBMESU : NOMBRE DE NOEUDS DE MESURE
!     IN  : NBMODE : NOMBRE DE MODES
!     IN  : NBABS : NOMBRE D ABSCISSES
!     IN  : PHI    : MATRICE MODALE REDUITE AUX NOEUDS DE MESURE
!     IN  : RMESU  : VALEURS DE MESURE
!     IN  : COEF   : COEFFICIENTS DE PONDERATION
!     IN  : XABS  : LISTE REELLE D ABSCISSES
!     IN  : LFONCT : =.TRUE. SI COEFFICIENTS DE PONDERATION
!                    DEPENDENT DE T
!     OUT : RETA    : DEPLACEMENT GENERALISE   ( MATRICE )
!     OUT : RETAP   : VITESSE  GENERALISEE     ( MATRICE )
!     OUT : RETA2P  : ACCELERATION GENERALISE  ( MATRICE )
!
    implicit none
!
!
!
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mtcrog.h"
#include "asterfort/rslsvd.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
    integer :: nbmesu, nbmode, nbabs
    integer :: vali
    real(kind=8) :: phi(nbmesu, nbmode)
    real(kind=8) :: valr
    real(kind=8) :: rmesu(nbmesu, nbabs), reta(nbmode, nbabs)
    real(kind=8) :: retap(nbmode, nbabs), reta2p(nbmode, nbabs)
    real(kind=8) :: xabs(nbabs)
    real(kind=8) :: coef(*)
    logical :: lfonct
! ----------------------------------------------------------------------
    integer :: imod, jmod, imes, iabs, ierr, ibid, jmes
    integer :: lscdmb, lwks, lphiph, lphitp, lmatsy, lwork, leta, lvals, lu, lv
    real(kind=8) :: alpha, eps
    logical :: nul
    character(len=3) :: method
    character(len=8) :: regul
    character(len=16) :: nomcha
    character(len=16) :: scdmbr, wks, phipht, phitph, matsys, work, eta, vals, u
    character(len=16) :: v
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! CREATION DES VECTEURS DE TRAVAIL
    scdmbr = '&SCDMBR'
    wks = '&WKS'
    phipht = '&PHIPHT'
    phitph = '&PHITPH'
    matsys = '&MATSYS'
    work = '&WORK'
    eta = '&ETA'
    vals = '&VALS'
    u = '&U'
    v = '&V'
!
    call wkvect(scdmbr, 'V V R', nbmode, lscdmb)
    call wkvect(wks, 'V V R', nbmode, lwks)
    call wkvect(phipht, 'V V R', nbmesu*nbmesu, lphiph)
    call wkvect(phitph, 'V V R', nbmode*nbmode, lphitp)
    call wkvect(matsys, 'V V R', nbmode*nbmode, lmatsy)
    call wkvect(work, 'V V R', nbmode, lwork)
    call wkvect(eta, 'V V R', nbmode, leta)
    call wkvect(vals, 'V V R', nbmode, lvals)
    call wkvect(u, 'V V R', nbmode*nbmode, lu)
    call wkvect(v, 'V V R', nbmode*nbmode, lv)
!
! METHODE DE RESOLUTION : LU / SVD
    call getvtx('RESOLUTION', 'METHODE', iocc=1, scal=method, nbret=ibid)
    if (ibid .eq. 0) method = 'LU'
!
    if (method .eq. 'SVD') then
        call getvr8('RESOLUTION', 'EPS', iocc=1, scal=eps, nbret=ibid)
        if (ibid .eq. 0) eps = 0.d0
    endif
!
! REGULARISATION : NON / NORM_MIN / TIK_RELA
    call getvtx('RESOLUTION', 'REGUL', iocc=1, scal=regul, nbret=ibid)
    if (ibid .eq. 0) regul = 'NON'
!
    call getvtx('MODELE_MESURE', 'NOM_CHAM', iocc=1, scal=nomcha, nbret=ibid)
!
! ===============================
! CALCUL DE PHI_TRANSPOSEE * PHI
! ===============================
    do 30 imod = 1, nbmode
        do 20 jmod = 1, nbmode
            zr(lphitp-1 +imod+nbmode*(jmod-1)) = 0.d0
            do 10 imes = 1, nbmesu
                zr(lphitp-1 +imod+nbmode*(jmod-1)) = zr(&
                                                     lphitp-1 + imod+nbmode*(jmod-1)) + phi(imes,&
                                                     imod)*phi(imes, jmod&
                                                     )
10          continue
20      continue
30  end do
!
    if (nbmesu .lt. nbmode) then
! ===============================
! CALCUL DE PHI * PHI_TRANSPOSEE
! ===============================
        do 40 imes = 1, nbmesu
            do 50 jmes = 1, nbmesu
                zr(lphiph-1 +imes+nbmesu*(jmes-1)) = 0.d0
                do 60 imod = 1, nbmode
                    zr(lphiph-1 +imes+nbmesu*(jmes-1)) = zr(&
                                                         lphiph-1 + imes+nbmesu*(jmes-1)&
                                                         ) + phi(imes,&
                                                         imod)*phi(jmes, imod&
                                                         )
60              continue
50          continue
40      continue
    endif
!
! =======================================
! CALCUL DE LA REPONSE GENERALISEE : RETA
! =======================================
!
! DEBUT DE LA BOUCLE SUR LES ABSCISSES
! *****************************
    do 100 iabs = 1, nbabs
!
        nul = .true.
!
! DEBUT DE LA BOUCLE SUR LES MODES
! ********************************
        do 90 imod = 1, nbmode
!
! RECHERCHE DU COEFFICIENT DE PONDERATION
! ***************************************
            if (lfonct) then
!             -> ALPHA DEPENDANT DES ABSCISSES
                alpha = coef (nbmode*(iabs-1)+imod)
            else
!             -> ALPHA INDEPENDANT DES ABSCISSES
                alpha = coef (imod)
            endif
!
!         -> ON VERIFIE QUE ALPHA > 0, SINON ARRET
            if (alpha .lt. 0.d0) then
                vali = iabs
                call u2mesg('F', 'ALGORITH15_24', 0, ' ', 1,&
                            vali, 0, 0.d0)
            else if (alpha .gt. r8prem()) then
                nul=.false.
            endif
!
! DETERMINATION DE LA MATRICE A INVERSER :
! MATSYS(IABS) = PHI_T*PHI + ALPHA(IABS)
! ****************************************
            do 80 jmod = 1, nbmode
                zr(lmatsy-1 +imod+nbmode*(jmod-1)) = zr( lphitp-1 + imod+nbmode*( jmod-1) )
80          continue
!
            zr(lmatsy-1 +imod+nbmode*(imod-1)) = zr( lmatsy-1 +imod+ nbmode*(imod-1 ) ) + alpha
!
! DETERMINATION DU SECOND MEMBRE :
! SCDMB(IABS) = PHI_T*Q + ALPHA(IABS)*RETA(IABS-1)
! RQ : A IABS=1, RETA(0)=0 (LA SOLUTION A PRIORI EST NULLE)
! ********************************************************
            zr(lscdmb-1 +imod) = 0.d0
!
            do 70 imes = 1, nbmesu
                zr(lscdmb-1 +imod) = zr(lscdmb-1 +imod) + phi(imes, imod)*rmesu(imes,iabs)
70          continue
!
            if ((regul .eq. 'TIK_RELA') .and. (iabs .gt. 1)) then
                zr(lscdmb-1 +imod) = zr(lscdmb-1 +imod) + alpha*reta( imod,iabs-1)
            endif
!
! FIN DE LA BOUCLE SUR LES MODES
! ******************************
90      continue
!
!
!
! RESOLUTION DU SYSTEME :
! MATSYS(IABS) * ETA(IABS) = SCDMB(IABS)
! **************************************
!
!       -> ALARME SI ALPHA NUL ET NBMESU<NBMODE : MOINDRE NORME
        if ((nbmesu .lt. nbmode) .and. (nul)) then
            call u2mesg('A', 'ALGORITH15_25', 0, ' ', 0,&
                        0, 0, 0.d0)
!
            if (regul .eq. 'NON') then
! CALCUL MOINDRE NORME
                call u2mesg('A', 'ALGORITH15_26', 0, ' ', 0,&
                            0, 0, 0.d0)
                do 82 imes = 1, nbmesu
                    zr(lscdmb-1 +imes) = rmesu(imes,iabs)
                    do 81 jmes = 1, nbmesu
                        zr(lmatsy-1 +imes+nbmode*(jmes-1)) = zr(lphiph-1 +imes+nbmesu*( jmes-1))
81                  continue
82              continue
!
! CHOIX POUR LA METHODE D INVERSION
                if (method .eq. 'SVD') then
! METHODE SVD
! CREATION DU VECTEUR SECOND MEMBRE
                    do 83 jmes = 1, nbmesu
                        zr(leta-1 +jmes) = zr(lscdmb-1 +jmes)
83                  continue
!
                    call rslsvd(nbmode, nbmesu, nbmesu, zr(lmatsy), zr( lvals),&
                                zr(lu), zr(lv), 1, zr(leta), eps,&
                                ierr, zr( lwork))
                    if (ierr .ne. 0) then
                        vali = iabs
                        valr = xabs ( iabs )
                        call u2mesg('F', 'ALGORITH15_27', 0, ' ', 1,&
                                    vali, 1, valr)
                    endif
!
                else
! METHODE DE CROUT
                    call mtcrog(zr(lmatsy), zr(lscdmb), nbmode, nbmesu, 1,&
                                zr(leta), zr(lwks), ierr)
                    if (ierr .ne. 0) then
                        vali = iabs
                        valr = xabs ( iabs )
                        call u2mesg('F', 'ALGORITH15_28', 0, ' ', 1,&
                                    vali, 1, valr)
                    endif
                endif
!
! COPIE DES RESULTATS DANS RETA
                do 84 jmod = 1, nbmode
                    do 85 jmes = 1, nbmesu
                        reta(jmod,iabs) = zr(leta-1 +jmes)*phi(jmes, jmod)
85                  continue
84              continue
!
                goto 100
            endif
        endif
! FIN CALCUL MOINDRE NORME
!
!
! CHOIX POUR LA METHODE DE RESOLUTION
        if (method .eq. 'SVD') then
! METHODE SVD
! CREATION DU VECTEUR SECOND MEMBRE
            do 71 jmod = 1, nbmode
                zr(leta-1 +jmod) = zr(lscdmb-1 +jmod)
71          continue
!
            call rslsvd(nbmode, nbmode, nbmode, zr(lmatsy), zr(lvals),&
                        zr(lu), zr(lv), 1, zr(leta), eps,&
                        ierr, zr(lwork))
            if (ierr .ne. 0) then
                vali = iabs
                valr = xabs ( iabs )
                call u2mesg('F', 'ALGORITH15_27', 0, ' ', 1,&
                            vali, 1, valr)
            endif
!
        else
! METHODE DE CROUT
!
            call mtcrog(zr(lmatsy), zr(lscdmb), nbmode, nbmode, 1,&
                        zr(leta), zr(lwks), ierr)
            if (ierr .ne. 0) then
                vali = iabs
                valr = xabs ( iabs )
                call u2mesg('F', 'ALGORITH15_28', 0, ' ', 1,&
                            vali, 1, valr)
            endif
        endif
!
! COPIE DES RESULTATS DANS RETA
        do 73 jmod = 1, nbmode
            reta(jmod,iabs) = zr(leta-1 +jmod)
73      continue
!
! FIN DE LA BOUCLE SUR LES ABSCISSES
! ***************************
100  end do
!
! ========================================
! CALCUL DE LA VITESSE GENERALISEE : RETAP
! ========================================
!
    do 200 iabs = 1, nbabs
        if (iabs .ne. nbabs) then
            do 210 imod = 1, nbmode
                retap(imod,iabs) = (&
                                   reta(imod,iabs+1) - reta(imod, iabs)) / (xabs(iabs+1) - xabs(i&
                                   &abs)&
                                   )
210          continue
        else
            do 220 imod = 1, nbmode
                retap(imod,iabs) = retap(imod,iabs-1)
220          continue
        endif
200  end do
!
! =============================================
! CALCUL DE L'ACCELERATION GENERALISEE : RETA2P
! =============================================
!
    do 300 iabs = 1, nbabs
        if (iabs .ne. nbabs) then
            do 310 imod = 1, nbmode
                reta2p(imod,iabs) = (&
                                    retap(imod,iabs+1) - retap(imod, iabs)) / (xabs(iabs+1) - xab&
                                    &s(iabs)&
                                    )
310          continue
        else
            do 320 imod = 1, nbmode
                reta2p(imod,iabs) = reta2p(imod,iabs-1)
320          continue
        endif
300  end do
!
    if (nomcha .eq. 'VITE') then
        do 101 iabs = 1, nbabs
            do 102 imod = 1, nbmode
                retap(imod,iabs) = reta(imod,iabs)
102          continue
101      continue
        do 103 iabs = 1, nbabs
            if (iabs .ne. nbabs) then
                do 104 imod = 1, nbmode
                    reta2p(imod,iabs) = (&
                                        retap(imod,iabs+1)-retap( imod,iabs)) / (xabs(iabs+1) - x&
                                        &abs(iabs)&
                                        )
104              continue
            else
                do 105 imod = 1, nbmode
                    reta2p(imod,iabs) = reta2p(imod,iabs-1)
105              continue
            endif
103      continue
! ON FAIT L HYPOTHESE QUE LE DEPLACEMENT INITIAL EST NUL
        do 106 imod = 1, nbmode
            reta(imod,1) = 0.d0
106      continue
        do 107 iabs = 2, nbabs
            do 108 imod = 1, nbmode
                reta(imod,iabs) = retap(imod,iabs)*(xabs(iabs)-xabs( iabs-1)) + reta(imod,(iabs-1&
                                  &))
108          continue
107      continue
    endif
!
    if (nomcha .eq. 'ACCE') then
        do 201 iabs = 1, nbabs
            do 202 imod = 1, nbmode
                reta2p(imod,iabs) = reta(imod,iabs)
202          continue
201      continue
! ON FAIT L HYPOTHESE QUE VITESSE INITIALE EST NULLE
        do 203 imod = 1, nbmode
            retap(imod,1) = 0.d0
203      continue
        do 204 iabs = 2, nbabs
            do 205 imod = 1, nbmode
                retap(imod,iabs)=reta2p(imod,iabs)*(xabs(iabs)-xabs(&
                iabs-1)) + retap(imod,(iabs-1))
205          continue
204      continue
! ON FAIT L HYPOTHESE QUE DEPLACEMENT INITIAL EST NUL
        do 206 imod = 1, nbmode
            reta(imod,1) = 0.d0
206      continue
        do 207 iabs = 2, nbabs
            do 208 imod = 1, nbmode
                reta(imod,iabs) = retap(imod,iabs)*(xabs(iabs)-xabs( iabs-1)) + reta(imod,(iabs-1&
                                  &))
208          continue
207      continue
    endif
!
! DESTRUCTION DES VECTEURS DE TRAVAIL
!
    call jedetr(scdmbr)
    call jedetr(wks)
    call jedetr(phipht)
    call jedetr(phitph)
    call jedetr(matsys)
    call jedetr(work)
    call jedetr(eta)
    call jedetr(vals)
    call jedetr(u)
    call jedetr(v)
!
    call jedema()
!
end subroutine

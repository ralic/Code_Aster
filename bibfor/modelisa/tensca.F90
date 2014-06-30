subroutine tensca(tablca, icabl, nbnoca, nbf0, f0,&
                  delta, typrel, trelax, xflu, xret,&
                  ea, rh1000, mu0, fprg, frco,&
                  frli, sa, regl)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!  DESCRIPTION : CALCUL DE LA TENSION LE LONG D'UN CABLE
!  -----------   APPELANT : OP0180 , OPERATEUR DEFI_CABLE_BP
!
!                EN SORTIE ON COMPLETE LA TABLE RESULTAT
!                LES LIGNES COMPLETEES CORRESPONDENT AU DERNIER CABLE
!                LES CASES RENSEIGNEES CORRESPONDENT AU PARAMETRE
!                <TENSION>
!
!  IN     : TABLCA : CHARACTER*19
!                    NOM DE LA TABLE DECRIVANT LES CABLES
!  IN     : ICABL  : INTEGER , SCALAIRE
!                    NUMERO DU CABLE
!  IN     : NBNOCA : INTEGER ,
!                    CONTIENT LE NOMBRE DE NOEUDS DU CABLE ETUDIE
!  IN     : NBF0   : INTEGER , SCALAIRE
!                    NOMBRE D'ANCRAGES ACTIFS DU CABLE (0, 1 OU 2)
!  IN     : F0     : REAL*8 , SCALAIRE
!                    VALEUR DE LA TENSION APPLIQUEE A L'UN OU AUX DEUX
!                    ANCRAGES ACTIFS DU CABLE
!  IN     : DELTA  : REAL*8 , SCALAIRE
!                    VALEUR DU RECUL DE L'ANCRAGE
!  IN     : TYPREL  : CHARACTER*24
!                    TYPE DE RELAXATION UTILISEE
!  IN     : TRELAX : REAL*8 , SCALAIRE
!                    VALEUR DE LA FONCTION CARACTERISANT L'EVOLUTION DE
!                    LA RELAXATION DE L'ACIER DANS LE TEMPS POUR BPEL
!                    OU NOMBRE D'HEURES POUR LA RELAXATION SI ETCC
!                    UTILE SI RELAX = .TRUE.
!  IN     : XFLU   : REAL*8 , SCALAIRE
!                    VALEUR DU TAUX DE PERTE DE TENSION PAR FLUAGE DU
!                    BETON, EN % DE LA TENSION INITIALE
!  IN     : XRET   : REAL*8 , SCALAIRE
!                    VALEUR DU TAUX DE PERTE DE TENSION PAR RETRAIT DU
!                    BETON, EN % DE LA TENSION INITIALE
!  IN     : EA     : REAL*8 , SCALAIRE
!                    VALEUR DU MODULE D'YOUNG DE L'ACIER
!  IN     : RH1000 : REAL*8 , SCALAIRE
!                    VALEUR DE LA RELAXATION A 1000 HEURES EN %
!  IN     : MU0    : REAL*8 , SCALAIRE
!                    VALEUR DU COEFFICIENT DE RELAXATION DE L'ACIER
!                    PRECONTRAINT POUR BPEL
!
!  IN     : FPRG     : REAL*8 , SCALAIRE
!                    VALEUR DE LA CONTRAINTE LIMITE ELASTIQUE DE L'ACIER
!  IN     : FRCO   : REAL*8 , SCALAIRE
!                    VALEUR DU COEFFICIENT DE FROTTEMENT EN COURBE
!                    (CONTACT ENTRE LE CABLE ACIER ET LE MASSIF BETON)
!  IN     : FRLI   : REAL*8 , SCALAIRE
!                    VALEUR DU COEFFICIENT DE FROTTEMENT EN LIGNE
!                    (CONTACT ENTRE LE CABLE ACIER ET LE MASSIF BETON)
!  IN     : SA     : REAL*8 , SCALAIRE
!                    VALEUR DE L'AIRE DE LA SECTION DROITE DU CABLE
!  IN     : REGL   : CHARACTER*4, INDICATION DU REGLEMENT UTILISE
!                    BPEL OU ETCC
!
!
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbexip.h"
#include "asterfort/tbexve.h"
#include "asterfort/tensk1.h"
#include "asterfort/tensk2.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=19) :: tablca
    character(len=4) :: regl
    character(len=24) :: typrel
    integer :: icabl, nbnoca, nbf0
    real(kind=8) :: f0, delta, trelax, xflu, xret, ea, rh1000, mu0, fprg, frco
    real(kind=8) :: frli, sa
!
!
! VARIABLES LOCALES
! -----------------
    integer :: ibid, idecno, ino, ipara, jabsc, jalph, jf,   nblign
    integer :: nbpara, n1, irt, jtabx, jtaby, nbval
    real(kind=8) :: df, flim, krelax, fi, f2
    complex(kind=8) :: cbid
    logical(kind=1) :: trouv1, trouv2, exi1, exi2
    character(len=3) :: k3b
    character(len=24) :: abscca, alphca
    character(len=8) :: ntable, k8b
    character(len=19) :: newtab
    character(len=24) :: tabx, taby
!
    character(len=24) :: param, parcr(2)
    integer, pointer :: tbnp(:) => null()
    character(len=24), pointer :: tblp(:) => null()
    data          param /'TENSION                 '/
    data          parcr /'ABSC_CURV               ',&
     &                     'ALPHA                   '/
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
    cbid=(0.d0,0.d0)
    ibid=0
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   TRAITEMENT DES CAS PARTICULIERS F0 = 0 OU PAS D'ANCRAGE ACTIF
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!     NBNO = NBNOCA(ICABL)
!
    call jeveuo(tablca//'.TBNP', 'L', vi=tbnp)
    nblign = tbnp(2)
    idecno = nblign - nbnoca
!
    if ((f0.eq.0.0d0) .or. (nbf0.eq.0)) then
        do 10 ino = 1, nbnoca
            call tbajli(tablca, 1, param, [ibid], [0.d0],&
                        [cbid], k3b, idecno+ino)
10      continue
        goto 9999
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   RECUPERATION DE L'ABSCISSE CURVILIGNE ET DE LA DEVIATION ANGULAIRE
!     CUMULEE LE LONG DU CABLE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    nbpara = tbnp(1)
    call jeveuo(tablca//'.TBLP', 'L', vk24=tblp)
    trouv1 = .false.
    trouv2 = .false.
    do 20 ipara = 1, nbpara
        if (tblp(1+4*(ipara-1)) .eq. parcr(1)) then
            trouv1 = .true.
            abscca = tblp(1+4*(ipara-1)+2)
            call jeveuo(abscca, 'L', jabsc)
        endif
        if (tblp(1+4*(ipara-1)) .eq. parcr(2)) then
            trouv2 = .true.
            alphca = tblp(1+4*(ipara-1)+2)
            call jeveuo(alphca, 'L', jalph)
        endif
        if (trouv1 .and. trouv2) goto 30
20  end do
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 4   CALCUL DE LA TENSION LE LONG DU CABLE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
30  continue
!
!
    call wkvect('&&TENSCA.F', 'V V R', nbnoca, jf)
!
! 4.1 CALCUL DE LA TENSION LE LONG DU CABLE EN PRENANT EN COMPTE LES
! --- PERTES PAR FROTTEMENT ET PAR RECUL DU(DES) ANCRAGE(S)
!    PAS DE DIFFERENCE ENTRE ETCC ET BPEL
!
    if (nbf0 .eq. 1) then
        call tensk1(icabl, nbnoca, zr(jabsc+idecno), zr(jalph+idecno), f0,&
                    delta, ea, frco, frli, sa,&
                    zr(jf))
    else
        call tensk2(icabl, nbnoca, zr(jabsc+idecno), zr(jalph+idecno), f0,&
                    delta, ea, frco, frli, sa,&
                    zr(jf))
    endif
!
!
! 4.2 PRISE EN COMPTE LE CAS ECHEANT DES PERTES DE TENSION PAR
! --- RELAXATION DE L'ACIER
!
    if (typrel .ne. 'SANS') then
        if (rh1000 .le. r8prem()) then
            call utmess('A', 'MODELISA2_70')
        endif
    endif
    if (typrel .eq. 'BPEL') then
!----------------------------------
!     CAS DU BPEL
!-----------------------
        flim = fprg * sa
        krelax = trelax * 5.0d-02 * rh1000
!
        do 40 ino = 1, nbnoca
            zr(jf+ino-1) = zr(jf+ino-1) * ( 1.0d0 - krelax * (zr(jf+ ino-1)/flim-mu0) )
40      continue
!
    else if (typrel.eq.'ETCC_DIRECT') then
!----------------------------------
!        CAS ETCC_DIRECT
!----------------------------------
        flim = fprg * sa
        do 45 ino = 1, nbnoca
            fi = zr(jf+ino-1)
            zr(jf+ino-1) = fi - 0.8d0 * fi * 0.66d-05 *rh1000*exp( 9.1d0*fi/flim)* (trelax/1000.d&
                           &0)**(0.75d0*(1.d0-(fi/flim) ))
!
45      continue
    else if (typrel.eq.'ETCC_REPRISE') then
!----------------------------------
!        CAS ETCC_REPRISE
!----------------------------------
        call getvid('DEFI_CABLE', 'TENSION_CT', iocc=icabl, scal=ntable, nbret=n1)
        if (n1 .eq. 0) then
            call utmess('F', 'MODELISA2_56')
        endif
!
        newtab=ntable
        tabx = '&&TENSCA_TABREF_CURV'
        taby = '&&TENSCA_TABREF_TENS'
!
        call jeexin(newtab//'.TBBA', irt)
        if (irt .eq. 0) then
            call utmess('F', 'UTILITAI4_64')
        endif
!     VERIFICATION DE LA PRESENCE DES BONS PARAMETRES
        call tbexip(newtab, 'ABSC_CURV', exi1, k8b)
        call tbexip(newtab, 'N', exi2, k8b)
!
        if (.not.exi1 .and. .not.exi2) then
            call utmess('F', 'MODELISA2_67')
        endif
!
        call tbexve(newtab, 'ABSC_CURV', tabx, 'V', nbval,&
                    k8b)
        call jeveuo(tabx, 'L', jtabx)
        call tbexve(newtab, 'N', taby, 'V', nbval,&
                    k8b)
        call jeveuo(taby, 'L', jtaby)
        if (nbval .ne. nbnoca) then
            call utmess('F', 'MODELISA2_68')
        endif
!     ON VERIFIE A MINIMA QUE LES ABSCISSES CURVILIGNES SONT IDENTIQUES
!     (MAIS PAS LES COORDONNES EXACTES)
        do 50 ino = 1, nbnoca
            if (zr(jtabx+ino-1)-zr(jabsc+ino-1) .ge. r8prem()) then
                call utmess('F', 'MODELISA2_69')
            endif
50      continue
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  MISE A JOUR DE LA TENSION
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
        do 60 ino = 1, nbnoca
            f2 = zr(jtaby+ino-1)
            zr(jf+ino-1) = zr(jf+ino-1) - 0.8d0 * 0.66d-05 *rh1000* exp(9.1d0*f2/fprg/sa)* (trela&
                           &x/1000.d0)**(0.75d0*(1.d0-( f2/fprg/sa) ))*f2
60      continue
!
!
        call jedetr(tabx)
        call jedetr(taby)
!
!
    endif
!
! 4.3 PRISE EN COMPTE LE CAS ECHEANT DES PERTES DE TENSION PAR
! --- FLUAGE ET RETRAIT DU BETON - UNIQUEMENT POUR BPEL
!
    if (regl .eq. 'BPEL') then
!
        if (xflu+xret .ne. 0.0d0) then
            df = ( xflu + xret ) * f0
            do 80 ino = 1, nbnoca
                zr(jf+ino-1) = zr(jf+ino-1) - df
80          continue
        endif
!
    endif
!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 5   MISE A JOUR DES OBJETS DE SORTIE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    do 90 ino = 1, nbnoca
        call tbajli(tablca, 1, param, [ibid], zr(jf+ino-1),&
                    [cbid], k3b, idecno+ ino)
90  end do
!
9999  continue
    call jedetr('&&TENSCA.F')
    call jedema()
!
! --- FIN DE TENSCA.
end subroutine

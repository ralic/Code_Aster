subroutine cremnl(reprise, baseno, numrep, nbordr0, nbordr, nbpt, neq,&
                  nbhar, imat, numedd, parcho, nbchoc, vk8, modrep)
! aslint: disable=W1306

!
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
!     -----------------------------------------------------------------
! IN REPRISE: REPRISE DE CALCUL (LOGICAL)
! IN BASENO : BASE NOM DES OBJETS DE TRAVAIL
! IN NBORDR : NOMBRE DE NUMEROS D ORDRE
! IN NBPT   : NOMBRE DE POINT DE DISCRETISATION
! IN NEQ    : NB EQUATIONS
! IN NBHAR  : NB HARMONIQUES
! IN IMAT   : DESCRIPTEUR DES MATRICES
!             IMAT(1) : MATRICE DE RIGIDITE
!             IAMT(2) : MATRICE DE MASSE
! IN NUMEDD : NUME_DDL DES MATRICES DU SYSTEME
! IN PARCHO : SD VOLATILE OU SONT STOCKES LES PARAMETRES DE CHOC
! IN NBCHOC : NB DE LIEUX DE CHOC
!     -----------------------------------------------------------------
    implicit none
#include "jeveux.h"
!     -----------------------------------------------------------------
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/utmess.h"
#include "blas/dnrm2.h"
#include "asterfort/gcncon.h"
#include "asterc/getres.h"
#include "blas/idamax.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterc/r8vide.h"
#include "asterfort/refdaj.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
    logical :: reprise, suite
    character(len=4) :: nomsym(1)
    character(len=8) :: nomres, nomrep, baseno, k8b, nomtab, modrep
    character(len=8) :: typpar(12), typpat(11), vk8
    character(len=14) :: parcho
    character(len=16) :: nomcmd, typres, valk(6), kbif
    character(len=19) :: nompar(12), chamno
    character(len=19) :: nompat(11)
    character(len=24) :: numedd, rigid, masse, valkt(4), matrice(3)
    integer :: iordr, numrep, nbordr0, nbordr, nbpt, vali(4), nbhar, nbsym, isort, lvale
    integer :: neq, imat(2), iadd, ieq, ier, ihar, ladpa
    integer :: nbpar, neqv, nmodes, iomega, valit(1), nbpart
    integer :: nbchoc, ityp, iraid, ireg, ijeu, inoe, icmp, iorig
    integer :: harmaxa, harmax, ibif, inspec
    real(kind=8) :: rvide, valr(2), freq, energ, xnorm, valrt(6)
    real(kind=8) :: espec(nbhar+1), nspec(neq)
    complex(kind=8) :: cvide
!     -----------------------------------------------------------------
    call jemarq()
    call getres(nomres, typres, nomcmd)   
!
    if (nomres .ne. modrep) then
        suite = .true.
    else
        suite = .false.
    endif

    rvide = r8vide()
    cvide = dcmplx(rvide,rvide)
!
    nbpar = 12
!
    nompar(1)='NUME_ORDRE'
    typpar(1)='I'
    nompar(2)='FREQUENCE'
    typpar(2)='R'
    nompar(3)='ENERGIE'
    typpar(3)='R'
    nompar(4)='NB_COEF_FOURIER'
    typpar(4)='I'
    nompar(5)='NOM_SD'
    typpar(5)='K8'
    nompar(6)='TYPE_OBJET'
    typpar(6)='K16'
    nompar(7)='NOM_OBJET'
    typpar(7)='K16'
    nompar(8)='CARA_CHOC'
    typpar(8)='K8'
    nompar(9)='HARM_MAX'
    typpar(9)='I'
    nompar(10)='BIFURCATION'
    typpar(10)='K16'
    nompar(11)='STABILITE'
    typpar(11)='K16'
    nompar(12)='NUME_REPRISE'
    typpar(12)='I'

    if ((.not. reprise) .or. (suite)) then
!-- INITIALISATION DE LA TABLE_CONTAINER
        call detrsd('TABLE_CONTAINER', nomres)
        call tbcrsd(nomres, 'G')
        call tbajpa(nomres, nbpar, nompar, typpar)
        call gcncon('_', nomtab)
    endif
!
    nbsym = 1
    nomsym(1) = 'DEPL'
!
    rigid=zk24(zi(imat(1)+1))
    masse=zk24(zi(imat(2)+1))
    neqv = zi(imat(1)+2)
    if (neq .ne. neqv) then
        call utmess('F', 'UTILITAI3_21')
    endif
!
    call jeveuo(baseno//'.SORTI', 'L', isort)
    call jeveuo(baseno//'.BIF', 'L', ibif)
!
    nmodes = 2*nbhar+1
!
!
!     BOUCLE SUR LES NUMEROS D ORDRE    
    do 100 iordr = 1, nbordr
!-- ATTRIBUTION D UN NOM DE CONCEPT
        call gcncon('_', nomrep)
!
        call rscrsd('G', nomrep, 'MODE_MECA', nmodes)
        matrice(1) = rigid
        matrice(2) = masse
        matrice(3) = ' '
        call refdaj ('F', nomrep,nmodes,numedd,'DYNAMIQUE',matrice,ier)
        do 200 ihar = 1, nmodes
            call rsexch(' ', nomrep, nomsym(1), ihar, chamno,&
                        ier)
            if (ier .eq. 0) then
            else if (ier .eq. 100) then
                call vtcrem(chamno, rigid, 'G', 'R')
            else
                call utmess('F', 'ALGELINE3_11')
            endif
!
            call jeveuo(chamno//'.VALE', 'E', lvale)
            xnorm = 0.d0
            do 44 ieq = 1, neq
                iadd = (iordr-1)*(neq*nmodes+2)+(ihar-1)*neq+ieq
                zr(lvale+ieq-1) = zr(isort-1+iadd)
                xnorm = xnorm + zr(isort-1+iadd)*zr(isort-1+iadd)
44          continue
            call rsnoch(nomrep, nomsym(1), ihar)
!
            call rsadpa(nomrep, 'E', 1, 'NUME_MODE', ihar,&
                        0, sjv=ladpa, styp=k8b)
            zi(ladpa) = ihar
!
            call rsadpa(nomrep, 'E', 1, 'FREQ', ihar,&
                        0, sjv=ladpa, styp=k8b)
!     -----------------------------------------------------------------
! reverifier le rangement des contributions harmoniques
! ici : 0 C1 C2  ... CH S1 S2 ... SH
!     -----------------------------------------------------------------
            if (ihar .eq. 1) then
                iomega = 0
            else if (ihar .le. nbhar+1) then
                iomega = ihar-1
            else
                iomega = ihar-(nbhar+1)
            endif
!
            iadd = (iordr-1)*(neq*nmodes+2)+nmodes*neq+1
            zr(ladpa) = iomega*zr(isort-1+iadd)
!
            call rsadpa(nomrep, 'E', 1, 'FACT_PARTICI_DX', ihar,&
                        0, sjv=ladpa, styp=k8b)
            zr(ladpa) = sqrt(xnorm)
200      continue
!
        iadd = (iordr-1)*(neq*nmodes+2)+neq*nmodes+1
        freq = zr(isort-1+iadd)
!
        iadd = (iordr-1)*(neq*nmodes+2)+neq*nmodes+2
        energ = zr(isort-1+iadd)
!
        call jelira(nomrep//'           .ORDR', 'LONUTI', nmodes, k8b)
!
!       DETERMINER HARMONIQUE MAX
        harmax=1
        do 440 ieq = 1, neq
            iadd = (iordr-1)*(neq*nmodes+2)+ieq
            nspec(ieq)=dnrm2(2*nbhar+1,zr(isort-1+iadd),neq)**2
440     continue
        inspec=idamax(neq,nspec,1)
        iadd = (iordr-1)*(neq*nmodes+2)+inspec
        espec(1)=zr(isort-1+iadd)**2/nspec(inspec)
        do 20 ihar = 1, nbhar
            espec(ihar+1)=(zr(isort-1+iadd+ihar*neq)**2+&
            zr(isort-1+iadd+(nbhar+ihar)*neq)**2)/nspec(inspec)
20      continue
        harmaxa=idamax(nbhar+1,espec,1)-1
        if(harmaxa.gt.harmax) then
            harmax=harmaxa
        endif
        if (zi(ibif+int(iordr/(nbpt-1))) .eq. 1) then
            kbif='OUI'
        else
            kbif='NON'
        endif
!
        if ((reprise) .and. (.not. suite)) then
            vali(1) = iordr+nbordr0
        else
            vali(1) = iordr
        endif
        vali(2) = nmodes
        vali(3) = harmax
        vali(4) = numrep
        valr(1) = freq
        valr(2) = energ
        valk(1) = nomrep
        valk(2) = 'MODE_MECA'
        valk(3) = 'COEF_FOURIER'
        if (reprise) then
            valk(4) = vk8
        else
            valk(4) = nomtab
        endif
        valk(5) = kbif
        valk(6) = 'NON_EVALUE'
        call tbajli(nomres, nbpar, nompar, vali, valr,&
                    cvide, valk, 0)
!
100  continue
!
    if (.not. reprise) then
! TABLE POUR LES CARACTERISTIQUES DE CHOC
        call tbcrsd(nomtab, 'G')
        nbpart = 11
        call jeveuo(parcho//'.RAID', 'L', iraid)
        call jeveuo(parcho//'.JEU', 'L', ijeu)
        call jeveuo(parcho//'.REG', 'L', ireg)
        call jeveuo(parcho//'.TYPE', 'L', ityp)
        call jeveuo(parcho//'.NOEU', 'L', inoe)
        call jeveuo(parcho//'.CMP', 'L', icmp)
        call jeveuo(parcho//'.ORIG', 'L', iorig)
!
        nompat(1)='NUME_CHOC'
        typpat(1)='I'
        nompat(2)='TYPE_CHOC'
        typpat(2)='K8'
        nompat(3)='NOEUD_CHOC'
        typpat(3)='K8'
        nompat(4)='NOM_CMP_1'
        typpat(4)='K8'
        nompat(5)='NOM_CMP_2'
        typpat(5)='K8'
        nompat(6)='RIGI_NOR'
        typpat(6)='R'
        nompat(7)='PARA_REGUL'
        typpat(7)='R'
        nompat(8)='JEU'
        typpat(8)='R'
        nompat(9)='ORIG_OBST_X'
        typpat(9)='R'
        nompat(10)='ORIG_OBST_Y'
        typpat(10)='R'
        nompat(11)='ORIG_OBST_Z'
        typpat(11)='R'
        call tbajpa(nomtab, nbpart, nompat, typpat)
!
        do 300 iordr = 1, nbchoc
            valit(1) = iordr
            valkt(1) = zk8(ityp-1+iordr)
            valkt(2) = zk8(inoe-1+iordr)
            valkt(3) = zk8(icmp-1+2*(iordr-1)+1)
            valkt(4) = zk8(icmp-1+2*(iordr-1)+2)
            valrt(1) = zr(iraid-1+iordr)
            valrt(2) = zr(ireg-1+iordr)
            valrt(3) = zr(ijeu-1+iordr)
            valrt(4) = zr(iorig-1+(iordr-1)*3+1)
            valrt(5) = zr(iorig-1+(iordr-1)*3+2)
            valrt(6) = zr(iorig-1+(iordr-1)*3+3)
            call tbajli(nomtab, nbpart, nompat, valit, valrt,&
                    cvide, valkt, 0)
300      continue
    endif
!
    call jedema()
end subroutine

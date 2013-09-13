subroutine jedebu(nbfi, mxzon, idb)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "jeveux_private.h"
#include "asterc/gtoptk.h"
#include "asterc/gtoptr.h"
#include "asterc/ismaem.h"
#include "asterc/isnnem.h"
#include "asterc/ispbem.h"
#include "asterc/lbisem.h"
#include "asterc/loc8em.h"
#include "asterc/lofiem.h"
#include "asterc/loisem.h"
#include "asterc/lolsem.h"
#include "asterc/lor8em.h"
#include "asterc/mofiem.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/jermxd.h"
#include "asterfort/jxdate.h"
#include "asterfort/utgtme.h"
#include "asterfort/utmess.h"
#include "asterfort/utptme.h"
    integer :: nbfi, mxzon, idb
! ----------------------------------------------------------------------
! ROUTINE UTILISATEUR D'INITIALISATION GENERALE POUR LE GESTIONNAIRE
!         DE MEMOIRE
!
! IN  NBFI   : NOMBRE MAXIMUM DE BASES SIMULTANEES ( =< 5)
! IN  MXZON  : LIMITE MEMOIRE DYNAMIQUE (EN ENTIER words)
! IN  IDB    : PARAMETRE DE DEBUG
!              ( 0: RIEN, 1: MISE A UNDEF DES SEGMENTS DE VALEURS )
!
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
    integer :: nbfic
    common /iparje/  nbfic
    integer :: iloc
    common /ilocje/  iloc
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, jcara, jdate, jdocu, jgenr, jhcod
    integer :: jiacce, jiadd, jiadm, jlong, jlono, jltyp
    integer :: jluti, jmarq, jorig, jrnom, jtype, k
    integer :: n, nbacce
    real(kind=8) :: val
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
!
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
! ----------------------------------------------------------------------
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
    logical :: litlec
    common /lficje/  litlec(n)
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
! ----------------------------------------------------------------------
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
! ----------------------------------------------------------------------
    integer :: isstat
    common /iconje/  isstat
    integer :: msstat, lsstat
    common /jconje/  msstat, lsstat
! ----------------------------------------------------------------------
    integer :: datei
    common /iheuje/  datei
! ----------------------------------------------------------------------
    integer :: illici, jclass(0:255)
    common /jchaje/  illici , jclass
! ----------------------------------------------------------------------
    integer :: istat
    common /istaje/  istat(4)
    character(len=4) :: kstat
    common /kstaje/  kstat
    integer :: mslois
    common /jenvje/  mslois
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
    integer :: idn, iext, nbenrg
    common /iextje/  idn(n) , iext(n) , nbenrg(n)
    integer :: lfic, mfic
    common /fenvje/  lfic,mfic
    character(len=128) :: repglo, repvol
    common /banvje/  repglo,repvol
    integer :: lrepgl, lrepvo
    common /balvje/  lrepgl,lrepvo
    integer :: lundef, idebug
    common /undfje/  lundef,idebug
    integer :: ldyn, lgdyn, nbdyn, nbfree
    common /idynje/  ldyn , lgdyn , nbdyn , nbfree
    integer :: icdyn, mxltot
    common /xdynje/  icdyn , mxltot
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2)
    real(kind=8) :: svuse, smxuse
    common /statje/  svuse,smxuse
    common /jiacce/  jiacce(n),nbacce(2*n)
! --------------------------------- ------------------------------------
    integer :: mxlici, iret
    real(kind=8) ::  rval(6)
    character(len=8) :: k8tab(6)
    parameter      ( mxlici = 67 )
    character(len=mxlici) :: clicit
    data clicit/' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.$&_abcdefghijklmnopqrstuvwxyz'/
! DEB ------------------------------------------------------------------
!
! ON AFFECTE ZI(1), ZR(1) ET ZC(1) AVEC UNE VALEUR QUI PEUT FAIRE
! PLANTER SI ELLE EST UTILISEE
!
    zi(1)=ismaem()
    zr(1)=r8nnem()
    zc(1)=dcmplx(zr(1),zr(1))
! -----------------  ENVIRONNEMENT MACHINE -----------------------------
    lfic = lofiem()
    call gtoptr('maxbase', val, iret)
    if (val .le. 0 .or. iret .ne. 0) then
        mfic = mofiem()
    else
        mfic = nint(val)*1024
    endif
    call gtoptk('repglob', repglo, iret)
    if (iret .ne. 0) then
        repglo='. '
        lrepgl=1
    else
        lrepgl=index(repglo,' ') - 1
        if (lrepgl .gt. 119) then
            call utmess('F', 'JEVEUX1_69', sk=repglo, si=lrepgl)
        endif
    endif
    call gtoptk('repvola', repvol, iret)
    if (iret .ne. 0) then
        repvol='. '
        lrepvo=1
    else
        lrepvo=index(repvol,' ') - 1
        if (lrepvo .gt. 119) then
            call utmess('F', 'JEVEUX1_70', sk=repvol, si=lrepvo)
        endif
    endif
    lbis = lbisem()
    lor8 = lor8em()
    loc8 = loc8em()
    lois = loisem()
    lols = lolsem()
    lundef = isnnem()
    mslois = lois - 1
    ldyn = 1
    lgdyn = 1
    mxdyn = 0
    lgio(1) = 0
    lgio(2) = 0
    mcdyn = 0
    mldyn = 0
    nbdyn = 0
    nbfree = 0
    icdyn = 0
    mxltot = 0
    svuse = 16
    smxuse = svuse
! -----------------  NOMBRE DE BASES -----------------------------------
    nbfic = min ( nbfi , n , len(classe) )
    ASSERT(nbfic .gt. 0 .and. nbfic .eq. nbfi)
! -----------------  CONSTANTES DE STATUT DES SEGMENTS DE VALEURS ------
    kstat = 'XUAD'
    isstat = ispbem( lbis - 3 )
    do 2 k = 1, 4
        istat(k) = k * isstat
 2  end do
    idebug = idb
! -----------------  ZONE MEMOIRE  -------------------------------------
    vmxdyn = mxzon
    if (mxzon .eq. 0) then
        vmxdyn = 1024
    endif
    vmet = vmxdyn
!
    call utptme(1, 'MEM_MUMP', 0.d0, iret)
    call utgtme(1, 'VMPEAK  ', rval, iret)
    if (rval(1) .le. 0) then
        call utmess('I', 'JEVEUX1_75')
    endif
    k8tab(1) = 'LIMIT_JV'
    k8tab(2) = 'MEM_TOTA'
    k8tab(3) = 'VMSIZE'
    k8tab(4) = 'CMAX_JV'
    k8tab(5) = 'COUR_JV'
    k8tab(6) = 'MEM_MUMP'
    call utgtme(6, k8tab, rval, iret)
!
    if (rval(3) .gt. 0) then
!
        call utptme(1, 'RLQ_MEM ', rval(3), iret)
        if (rval(1)-rval(3) .le. 0) then
            call utmess('F', 'JEVEUX1_71', nr=3, valr=rval)
        endif
        call jermxd((rval(1)-rval(3))*1024*1024, iret)
    endif
!
    liszon = 1
    jiszon = 1
    lk1zon = liszon * lois
    jk1zon = jiszon * lois
    iloc = loc ( iszon(jiszon) )
! -------------------  POINTEURS D'ATTRIBUTS  --------------------------
    do 5 i = 1, len(classe)
        classe(i:i) = '$'
 5  end do
    do 10 i = 1, nbfic
        jgenr(i) = 0
        jtype(i) = 0
        jltyp(i) = 0
        jdocu(i) = 0
        jorig(i) = 0
        jrnom(i) = 0
        jlono(i) = 0
        jlong(i) = 0
        jdate(i) = 0
        jiadd(i) = 0
        jiadm(i) = 0
        jmarq(i) = 0
        jluti(i) = 0
        jcara(i) = 0
        jhcod(i) = 0
        nremax(i) = 0
        nrhcod(i) = 0
        nreuti(i) = 0
        nblmax(i) = 0
        nbenrg(i) = 1
        nbluti(i) = 0
        longbl(i) = 0
        kitlec(i) = 0
        kitecr(i) = 0
        kiadm (i) = 0
        iitlec(i) = 0
        iitecr(i) = 0
        nitecr(i) = 0
        litlec(i) = .false.
        nomfic(i) = '        '
        kstout(i) = '        '
        kstini(i) = '        '
        classe(i:i) = ' '
        dn2(i) = ' '
        nbacce(2*i-1) = 0
        nbacce(2*i ) = 0
10  end do
! -------------------  CONSTANTES DE GESTION  --------------------------
    lsstat = lbis-4
    msstat = 0
    do 20 k = 1, lbis-4
        msstat = msstat + ispbem(k)
20  end do
    bl32 = ' '
!
    call jxdate(datei)
!
    illici = -1
    do 30 k = 0, 255
        jclass(k) = illici
30  end do
    do 31 k = 1, mxlici
        jclass(ichar( clicit(k:k) ) ) = k
31  end do
!
    kdesma(1) = 0
    kdesma(2) = 0
    lgduti = 0
    kposma(1) = 0
    kposma(2) = 0
    lgputi = 0
    ipgc = 0
! FIN ------------------------------------------------------------------
end subroutine

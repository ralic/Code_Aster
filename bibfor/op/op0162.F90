subroutine op0162()
    implicit none
!     ------------------------------------------------------------------
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
!     INTERFACE ASTER - MISS3D : PROCEDURE  IMPR_MISS_3D
!     ------------------------------------------------------------------
!
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/chpver.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ulisop.h"
#include "asterfort/ulopen.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
    integer :: ierd, gd
    character(len=8) :: k8b, resu, masse, noma, vect, typi
    character(len=14) :: nume
    character(len=16) :: concep, nomcmd, k16nom
    character(len=19) :: fonc
    character(len=24) :: refe, vale, deeq, type
    character(len=24) :: kbid, nomch0
    character(len=8) :: mael, basemo, nomfon, interf
    character(len=80) :: titre
    real(kind=8) :: petir8, di(6)
    real(kind=8) :: tini, tfin, fini, ffin, pas
    complex(kind=8) :: cbid
    integer :: iarg
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iadmo1, iadmo2, ibid, ic, idbase, iddeeq
    integer :: idvec1, idvec2, ifm, ifmis, iret, isvect, j
    integer :: j2, jfonc, jrefe, jtyp, jval, jvect, k
    integer :: n, n1, n2, n3, n4, nbmode, nbmods
    integer :: nbmodt, nbnoeu, nbval, nc, ncharb, nchars, neq
    integer :: nfo, niv, nmm, nsourf, nsours, nti, nu
!
    real(kind=8) :: coef, pij, t
!-----------------------------------------------------------------------
    data  refe  /'                  _REFE'/
    data  vale  /'                   .VALE'/
    data  nomch0 /'&&OP0162.CHAMNO'/
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call getres(resu, concep, nomcmd)
!
    call infmaj()
    call infniv(ifm, niv)
    petir8 = 1.d-40
!
!     ----- RECUPERATION UNITE DE MISS ---
    call getvis(' ', 'UNITE', 1, iarg, 1,&
                ifmis, nu)
    k16nom = ' '
    if (ulisop ( ifmis, k16nom ) .eq. 0) then
        call ulopen(ifmis, ' ', ' ', 'NEW', 'O')
    endif
!
!     ----- RECUPERATION DES MODES -----
    call getvid(' ', 'MACR_ELEM_DYNA', 1, iarg, 1,&
                mael, nmm)
    refe(1:18) = mael//'.MAEL_MASS'
    call jeveuo(refe, 'L', jrefe)
    basemo = zk24(jrefe)
    masse = zk24(jrefe+1)
!
!     --- ON RECUPERE LE TYPE D'INTERFACE ---
!
    call jeveuo(basemo(1:8)//'           .REFD', 'L', jval)
    interf = zk24(jval+4) (1:8)
    if (interf .ne. ' ') then
!       CALL BMNBMD(BASEMO,'MODE',NBMODE)
!       CALL BMNBMD(BASEMO,'DEFORMEE',NBMODS)
        call dismoi('F', 'NB_MODES_STA', basemo, 'RESULTAT', nbmods,&
                    k8b, ierd)
        call dismoi('F', 'NB_MODES_DYN', basemo, 'RESULTAT', nbmode,&
                    k8b, ierd)
        type = interf//'.IDC_TYPE'
        call jeveuo(type, 'L', jtyp)
        typi = zk8(jtyp)
    else
!       CALL JEVEUO(BASEMO//'           .UTIL','L',JVAL)
!       NBMODE = ZI(JVAL+2)
!       NBMODS = ZI(JVAL+3)
        call dismoi('F', 'NB_MODES_STA', basemo, 'RESULTAT', nbmods,&
                    k8b, ierd)
        call dismoi('F', 'NB_MODES_DYN', basemo, 'RESULTAT', nbmode,&
                    k8b, ierd)
        typi = 'CRAIGB'
    endif
    nbmodt = nbmode + nbmods
!
    write(ifm,'(1X,I6,1X,''MODES DYNAMIQUES'',1X,A8)') nbmode,typi
    write(ifm,'(1X,I6,1X,''MODES STATIQUES'',2X,A8)') nbmods,typi
    write(ifmis,'(''DYNA'',1X,I6,1X,A8)') nbmode,typi
    write(ifmis,'(''STAT'',1X,I6,1X,A8)') nbmods,typi
!
    call getvtx(' ', 'TITRE', 1, iarg, 1,&
                titre, nti)
    if (nti .ne. 0) write(ifmis,'(''TITRE'',/A80)') titre
    if (nti .ne. 0) write(ifm,'(A80)') titre
!
!
!--------RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
!
    call dismoi('F', 'NOM_NUME_DDL', masse, 'MATR_ASSE', ibid,&
                nume, ierd)
    call dismoi('F', 'NB_EQUA', masse, 'MATR_ASSE', neq,&
                kbid, iret)
    deeq = nume//'.NUME.DEEQ'
    call jeveuo(deeq, 'L', iddeeq)
    call dismoi('F', 'NOM_MAILLA', masse, 'MATR_ASSE', ibid,&
                noma, ierd)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoeu,&
                k8b, ierd)
    call dismoi('F', 'NUM_GD_SI', nume, 'NUME_DDL', gd,&
                k8b, ierd)
    if (interf .eq. ' ') call vtcreb(nomch0, nume, 'V', 'R', neq)
!
    call wkvect('&&OP0162.VECTASS1', 'V V R', neq, idvec1)
    call wkvect('&&OP0162.VECTASS2', 'V V R', neq, idvec2)
!
!     ----- RECUPERATION INSTANTS OU FREQUENCES ---
    call getvr8(' ', 'INST_INIT', 1, iarg, 1,&
                tini, n1)
    call getvr8(' ', 'INST_FIN', 1, iarg, 1,&
                tfin, n2)
    call getvr8(' ', 'FREQ_INIT', 1, iarg, 1,&
                fini, n3)
    call getvr8(' ', 'FREQ_FIN', 1, iarg, 1,&
                ffin, n4)
    call getvr8(' ', 'PAS', 1, iarg, 1,&
                pas, n)
    if (n1 .ne. 0) then
        write(ifmis,'(''TEMPS DE'',1X,1PE12.5,1X,''A'',1X,1PE12.5,&
     &  1X,''PAS'',1X,1PE12.5)') tini,tfin,pas
        write(ifm,'(''TEMPS DE'',1X,1PE12.5,1X,''A'',1X,1PE12.5,&
     &  1X,''PAS'',1X,1PE12.5)') tini,tfin,pas
    else
        write(ifmis,'(''FREQ DE'',1X,1PE12.5,1X,''A'',1X,1PE12.5,&
     &  1X,''PAS'',1X,1PE12.5)') fini,ffin,pas
        write(ifm,'(''FREQ DE'',1X,1PE12.5,1X,''A'',1X,1PE12.5,&
     &  1X,''PAS'',1X,1PE12.5)') fini,ffin,pas
    endif
    ic=0
    call getvr8(' ', 'DIRE_ONDE', ic, iarg, 3,&
                di(1), n)
    if (n .ne. 0) then
        write(ifmis,'(''DIRE ONDE'')')
        write(ifmis,'(3(1X,1PE12.5))') (di(i),i=1,3)
    endif
!     ----- RECUPERATION NIVEAU IMPRESSION ---
!
    call getfac('EXCIT', ncharb)
    write(ifmis,'(''CHARGE'',1X,I6)') ncharb
    write(ifm,'(''CHARGE'',1X,I6)') ncharb
    if (ncharb .eq. 0) goto 9998
    if (nbmode .eq. 0) then
        call wkvect('&&OP0162.DVECT', 'V V R', 1, jvect)
    else
        call wkvect('&&OP0162.DVECT', 'V V R', nbmode, jvect)
    endif
    if (nbmods .eq. 0) then
        call wkvect('&&OP0162.SVECT', 'V V R', 1, isvect)
    else
        call wkvect('&&OP0162.SVECT', 'V V R', nbmods, isvect)
    endif
    call wkvect('&&OP0162.BASEMO', 'V V R', nbmodt*neq, idbase)
    call copmod(basemo, 'DEPL', neq, nume, nbmodt,&
                'R', zr(idbase), cbid)
!
! --- ALLOCATION VECTEUR DE TRAVAIL
!
    call wkvect('&&OP0162.VECT1', 'V V R', neq, iadmo1)
    call wkvect('&&OP0162.VECT2', 'V V R', neq, iadmo2)
    do 70 ic = 1, ncharb
        call getvid('EXCIT', 'VECT_ASSE', ic, iarg, 1,&
                    vect, n)
        if (n .ne. 0) then
            call chpver('F', vect, 'NOEU', 'DEPL_R', ierd)
        endif
        vale(1:8) = vect
        call jeveuo(vale, 'L', idvec1)
        do 71 j = 1, nbmode
!
!-----      RECOPIE DU JEME MODE
!
            call dcopy(neq, zr(idbase+(j-1)*neq), 1, zr(idvec2), 1)
!
!-----       MISE A ZERO DES DDLS DE LAGRANGE
!
            call zerlag('R', zr(idvec2), cbid, neq, zi(iddeeq))
!
!-----       PRODUIT SCALAIRE VECTASS * MODE
!
            pij = ddot(neq,zr(idvec1),1,zr(idvec2),1)
            zr(jvect+j-1) = pij + petir8
71      continue
        do 72 j = 1, nbmods
!
!-----      RECOPIE DU JEME MODE
!
            j2 = j + nbmode
            call dcopy(neq, zr(idbase+(j2-1)*neq), 1, zr(idvec2), 1)
!
!-----       MISE A ZERO DES DDLS DE LAGRANGE
!
            call zerlag('R', zr(idvec2), cbid, neq, zi(iddeeq))
!
!-----       PRODUIT SCALAIRE VECTASS * MODE
!
            pij = ddot(neq,zr(idvec1),1,zr(idvec2),1)
            zr(isvect+j-1) = pij + petir8
72      continue
        if (niv .gt. 1) write(ifm,'(''DYNA CHAR'',1X,I6)') ic
        if (niv .gt. 1) write(ifm,'(6(1X,1PE12.5))') (zr(jvect+k-1),k=1, nbmode )
        if (niv .gt. 1) write(ifm,'(''STAT CHAR'',1X,I6)') ic
        if (niv .gt. 1) write( ifm,'(6(1X,1PE12.5))') (zr(isvect+k-1),k= 1,nbmods)
        write(ifmis,'(''DYNA CHAR'',1X,I6)') ic
        write(ifmis,'(6(1X,1PE12.5))') (zr(jvect+k-1),k=1,nbmode)
        write(ifmis,'(''STAT CHAR'',1X,I6)') ic
        write(ifmis,'(6(1X,1PE12.5))') (zr(isvect+k-1),k=1,nbmods)
        call getvid('EXCIT', 'FONC_MULT', ic, iarg, 1,&
                    nomfon, nfo)
        if (nfo .ne. 0) goto 80
        coef = 1.d0
        t = 0.d0
        call getvr8('EXCIT', 'COEF_MULT', ic, iarg, 1,&
                    coef, nc)
        nbval = 1
        if (niv .gt. 1) write(ifm, '(''FONC CHAR'',1X,I6, ''VALE'',1X,I6)') ic, nbval
        if (niv .gt. 1) write(ifm,'(6(1X,1PE12.5))') t, coef
        write(ifmis,'(''FONC CHAR'',2(1X,I6))') ic,nbval
        write(ifmis,'(6(1X,1PE12.5))') t, coef
        goto 81
80      continue
        fonc = nomfon
        call jelira(fonc//'.VALE', 'LONMAX', nbval)
        call jeveuo(fonc//'.VALE', 'L', jfonc)
        nbval = nbval/2
        if (niv .gt. 1) write(ifm, '(''FONC CHAR'',1X,I6, ''VALE'',1X,I6)') ic, nbval
        if (niv .gt. 1) write(&
                        ifm, '(6(1X,1PE12.5))') (zr(jfonc+k-1), zr( jfonc+k+nbval-1), k=1, nbval)
        write(ifmis,'(''FONC CHAR'',2(1X,I6))') ic,nbval
        write(ifmis,'(6(1X,1PE12.5))') (zr(jfonc+k-1),zr(jfonc+k+&
        nbval-1),k=1,nbval)
81      continue
70  end do
9998  continue
    call getfac('EXCIT_SOL', nchars)
    write(ifmis,'(''SOLS'',1X,I6)') nchars
    write(ifm,'(''SOLS'',1X,I6)') nchars
    if (nchars .eq. 0) goto 9999
    do 73 ic = 1, nchars
        call getvr8('EXCIT_SOL', 'DIRECTION', ic, iarg, 3,&
                    di(1), n)
        call getvtx('EXCIT_SOL', 'NOM_CHAM', ic, iarg, 1,&
                    typi, n)
        if (niv .gt. 1) write(ifm,'(''DIRE SOLS'',1X,I6)') ic
        if (niv .gt. 1) write(ifm,'(3(1X,1PE12.5))') (di(i),i=1,3)
        if (niv .gt. 1) write(ifm, '(''SOLS'',1X,I6,1X, ''TYPE'',1X,A8)') ic, typi
        write(ifmis,'(''DIRE SOLS'',1X,I6)') ic
        write(ifmis,'(3(1X,1PE12.5))') (di(i),i=1,3)
        write(ifmis,'(''TYPE SOLS'',1X,I6,1X,A8)') ic,typi
        call getvid('EXCIT_SOL', 'FONC_SIGNAL', ic, iarg, 1,&
                    nomfon, n)
        fonc = nomfon
        call jelira(fonc//'.VALE', 'LONMAX', nbval)
        call jeveuo(fonc//'.VALE', 'L', jfonc)
        nbval = nbval/2
        if (niv .gt. 1) write(ifm, '(''FONC SOLS'',1X,I6,1X, ''VALE'',1X,I6)' ) ic, nbval
        if (niv .gt. 1) write(&
                        ifm, '(6(1X,1PE12.5))') (zr(jfonc+k-1), zr( jfonc+k+nbval-1), k=1, nbval)
        write(ifmis,'(''FONC SOLS'',2(1X,I6))') ic,nbval
        write(ifmis,'(6(1X,1PE12.5))') (zr(jfonc+k-1),zr(jfonc+k+&
        nbval-1),k=1,nbval)
73  end do
9999  continue
!
    call getfac('SOURCE_SOL', nsours)
    if (nsours .eq. 0) goto 9995
    write(ifmis,'(''SOUS'',1X,I6)') nsours
    write(ifm,'(''SOUS'',1X,I6)') nsours
    do 74 ic = 1, nsours
!         IC = 1
        call getvr8('SOURCE_SOL', 'POINT', ic, iarg, 3,&
                    di(1), n)
        call getvr8('SOURCE_SOL', 'DIRECTION', ic, iarg, 3,&
                    di(4), n)
        call getvtx('SOURCE_SOL', 'NOM_CHAM', ic, iarg, 1,&
                    typi, n)
        if (niv .gt. 1) write(ifm,'(''DIRE SOUS'',1X,I6)') ic
        if (niv .gt. 1) write(ifm,'(3(1X,1PE12.5))') (di(i),i=1,6)
        if (niv .gt. 1) write(ifm, '(''SOLS'',1X,I6,1X, ''TYPE'',1X,A8)') ic, typi
        write(ifmis,'(''DIRE SOUS'',1X,I6)') ic
        write(ifmis,'(3(1X,1PE12.5))') (di(i),i=1,6)
        write(ifmis,'(''TYPE SOUS'',1X,I6,1X,A8)') ic,typi
        call getvid('SOURCE_SOL', 'FONC_SIGNAL', ic, iarg, 1,&
                    nomfon, n)
        fonc = nomfon
        call jelira(fonc//'.VALE', 'LONMAX', nbval)
        call jeveuo(fonc//'.VALE', 'L', jfonc)
        nbval = nbval/2
        if (niv .gt. 1) write(ifm, '(''FONC SOUS'',1X,I6,1X, ''VALE'',1X,I6)' ) ic, nbval
        if (niv .gt. 1) write(&
                        ifm, '(6(1X,1PE12.5))') (zr(jfonc+k-1), zr( jfonc+k+nbval-1), k=1, nbval)
        write(ifmis,'(''FONC SOUS'',2(1X,I6))') ic,nbval
        write(ifmis,'(6(1X,1PE12.5))') (zr(jfonc+k-1),zr(jfonc+k+&
        nbval-1),k=1,nbval)
74  end do
9995  continue
!
    call getfac('SOURCE_FLUIDE', nsourf)
    if (nsourf .eq. 0) goto 9996
    write(ifmis,'(''SOUF'',1X,I6)') nsourf
    write(ifm,'(''SOUF'',1X,I6)') nsourf
    do 75 ic = 1, nsourf
!         IC = 1
        call getvr8('SOURCE_FLUIDE', 'POINT', ic, iarg, 3,&
                    di(1), n)
        call getvtx('SOURCE_FLUIDE', 'NOM_CHAM', ic, iarg, 1,&
                    typi, n)
        if (niv .gt. 1) write(ifm,'(''DIRE SOUF'',1X,I6)') ic
        if (niv .gt. 1) write(ifm,'(3(1X,1PE12.5))') (di(i),i=1,3)
        if (niv .gt. 1) write(ifm, '(''SOLS'',1X,I6,1X, ''TYPE'',1X,A8)') ic, typi
        write(ifmis,'(''DIRE SOUF'',1X,I6)') ic
        write(ifmis,'(3(1X,1PE12.5))') (di(i),i=1,3)
        write(ifmis,'(''TYPE SOUF'',1X,I6,1X,A8)') ic,typi
        call getvid('SOURCE_FLUIDE', 'FONC_SIGNAL', ic, iarg, 1,&
                    nomfon, n)
        fonc = nomfon
        call jelira(fonc//'.VALE', 'LONMAX', nbval)
        call jeveuo(fonc//'.VALE', 'L', jfonc)
        nbval = nbval/2
        if (niv .gt. 1) write(ifm, '(''FONC SOUF'',1X,I6,1X, ''VALE'',1X,I6)' ) ic, nbval
        if (niv .gt. 1) write(&
                        ifm, '(6(1X,1PE12.5))') (zr(jfonc+k-1), zr( jfonc+k+nbval-1), k=1, nbval)
        write(ifmis,'(''FONC SOUF'',2(1X,I6))') ic,nbval
        write(ifmis,'(6(1X,1PE12.5))') (zr(jfonc+k-1),zr(jfonc+k+&
        nbval-1),k=1,nbval)
75  end do
9996  continue
!
    call jedema()
end subroutine

subroutine aceamr(noma, nomo, lmax, noemaf, nbocc,&
                  ivr, ifm)
    implicit      none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvtx.h"
#include "asterfort/affdis.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/infdis.h"
#include "asterfort/infniv.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/masrep.h"
#include "asterfort/nocart.h"
#include "asterfort/r8inir.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    integer :: ifm, lmax, noemaf, nbocc, ivr(*)
    character(len=8) :: noma, nomo
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
!
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET PAR
!     MASSE REPARTIE
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
! IN  : LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
! IN  : NBOCC  : NOMBRE D'OCCURRENCES DU MOT CLE MASS_AJOU
! IN  : IVR    : TABLEAU DES INDICES DE VERIFICATION
! ----------------------------------------------------------------------
!
    integer :: nbcar, nbval, nrd
    parameter    ( nbcar = 100 , nbval = 6 , nrd = 2 )
    integer :: jdc(3), jdv(3), ibid, niv, iunite
    integer :: jdcinf, jdvinf
    integer :: i, ier, ii, in, inbn, ino, inoe, ioc, irep
    integer :: irgno, irgto, isym, itbmp, itbno, iv
    integer :: ixci, ixckma, jd, jdls, jj, jn
    integer :: l, ldgm, ldnm, lokm, lorep, nbnma
    integer :: nbno, nbnoeu, nborm, nc, ncarac, ncmp, nborp
    integer :: ndim, ng, ngp, nma, dimcar
    integer :: vali(2), nbval2
!
    real(kind=8) :: eta, vale(nbval), r8bid
    character(len=1) :: kma(3)
    character(len=8) :: nomnoe, nommai, k8bid, nomu, car(nbcar), lamass
    character(len=16) :: rep, repdis(nrd), concep, cmd
    character(len=19) :: cart(3), cartdi
    character(len=24) :: tmpnd(3), tmpvd(3), k24bid, nogp
    character(len=24) :: mlgnno, mlgnma, tmcinf, tmvinf
!
!
    logical :: transl, lvale
    integer :: iarg
    data repdis  /'GLOBAL          ','LOCAL           '/
    data kma     /'K','M','A'/
!     ------------------------------------------------------------------
!
    call jemarq()
    call getres(nomu, concep, cmd)
    nbval2 = 3
!
    mlgnno = noma//'.NOMNOE'
    mlgnma = noma//'.NOMMAI'
    call wkvect('&&TMPDISCRET', 'V V K24', lmax, jdls)
    call wkvect('&&TMPTABNO', 'V V K8', lmax, itbno)
    call wkvect('&&TMPRIGNO', 'V V R', 6*lmax, irgno)
    call wkvect('&&TMPRIGTO', 'V V R', 6*noemaf, irgto)
    call wkvect('&&TMPTABMP', 'V V K8', lmax, itbmp)
!
!
    ifm = iunifi('MESSAGE')
!
! --- RECUPERATION DE LA DIMENSION GEOMETRIQUE DU MODELE
    call dismoi('F', 'DIM_GEOM', nomo, 'MODELE', ibid,&
                k8bid, ier)
    ndim=ibid
    if (ibid .ge. 100) then
        ibid = ibid - 100
        ndim=1
    endif
    if (ibid .ge. 20) then
        ibid = ibid - 20
        ndim=2
    endif
    if (ibid .eq. 3) ndim=3
!     POUR LES DISCRETS C'EST OBLIGATOIREMENT DU 2D OU 3D
    call assert((ndim.eq.2).or.(ndim.eq.3))
!
! --- CONSTRUCTION DES CARTES ET ALLOCATION
    cartdi = nomu//'.CARDINFO'
    tmcinf = cartdi//'.NCMP'
    tmvinf = cartdi//'.VALV'
!     SI LA CARTE N'EXISTE PAS ON LA CREE
    call jeexin(tmcinf, ixci)
    if (ixci .eq. 0) call alcart('G', cartdi, noma, 'CINFDI')
    call jeveuo(tmcinf, 'E', jdcinf)
    call jeveuo(tmvinf, 'E', jdvinf)
!     PAR DEFAUT POUR M, A, K :
!        REPERE GLOBAL, MATRICE SYMETRIQUE, PAS AFFECTEE
    call infdis('DIMC', dimcar, r8bid, k8bid)
    do 200 i = 1, 3
        zk8(jdcinf+i-1) = 'REP'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i-1), zk8(jdcinf+i-1))
        zk8(jdcinf+i+2) = 'SYM'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i+2), zk8(jdcinf+i+2))
        zk8(jdcinf+i+5) = 'DIS'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i+5), zk8(jdcinf+i+5))
200  end do
    zk8(jdcinf+9) = 'ETAK    '
    call infdis('INIT', ibid, zr(jdvinf+9), zk8(jdcinf+9))
    zk8(jdcinf+10) = 'TYDI    '
    call infdis('INIT', ibid, zr(jdvinf+10), zk8(jdcinf+10))
! --- CREATION DES CARTES
    do 220 i = 1, 3
        cart(i) = nomu//'.CARDISC'//kma(i)
        tmpnd(i) = cart(i)//'.NCMP'
        tmpvd(i) = cart(i)//'.VALV'
!        SI LES CARTES N'EXISTENT PAS ON LES CREE
        call jeexin(tmpnd(i), ixckma)
        if (ixckma .eq. 0) then
            call alcart('G', cart(i), noma, 'CADIS'//kma(i))
        endif
        call jeveuo(tmpnd(i), 'E', jdc(i))
        call jeveuo(tmpvd(i), 'E', jdv(i))
220  end do
!
!     RECUPERATION DU NIVEAU D'IMPRESSION
    call infniv(ibid, niv)
!
! --- BOUCLE SUR LES OCCURRENCES DE MASS_AJOU
    do 30 ioc = 1, nbocc
        eta = 0.0d0
!        PAR DEFAUT ON EST DANS LE REPERE GLOBAL, MATRICES SYMETRIQUES
        irep = 1
        isym = 1
        rep = repdis(1)
        lvale=.false.
!
        call getvem(noma, 'GROUP_MA', 'MASS_AJOU', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
!         CALL GETVR8('MASS_AJOU','VALE'    ,IOC,IARG,NBVAL,VALE,NVAL)
        call r8inir(nbval, 0.0d0, vale, 1)
!         CALL GETVTX('MASS_AJOU','REPERE'  ,IOC,IARG,1,REP,NREP)
        call getvtx('MASS_AJOU', 'GROUP_MA_POI1', ioc, iarg, 1,&
                    nogp, ngp)
!
        do 32 i = 1, nrd
            if (rep .eq. repdis(i)) irep = i
32      continue
!        UNITE POUR IMPRIMER LES VALEUR DES DISCRETS
!         CALL GETVIS('MASS_AJOU','UNITE',IOC,IARG,1,IBID,IER)
        iunite = 6
!
        ncarac=1
        if (iunite .gt. 0) then
            write(iunite,1000) rep,ioc
        endif
!
! ---    "GROUP_MA" = TOUTES LES MAILLES DE TOUS LES GROUPES DE MAILLES
        if (ng .le. 0) goto 30
        car(1)='M_T_N'
!
!         II = 0
        do 34 nc = 1, ncarac
            transl=.true.
!
            if (transl) then
                lamass = 'K_T_D_N'
                call masrep(noma, ioc, vale, lvale, ng,&
                            zk24(jdls), nbno, zk8(itbno), zr(irgno), zr(irgto),&
                            ndim)
            else
                call assert(.false.)
            endif
!
            do 255 ino = 1, nbno
                zk8(itbmp + ino - 1) = ' '
255          continue
!
!
!
            if (ngp .ne. 0) then
                nbnoeu = 1
                lokm = 5
!
                call jelira(jexnom(noma//'.GROUPEMA', nogp), 'LONMAX', nma, k24bid)
                call jeveuo(jexnom(noma//'.GROUPEMA', nogp), 'L', ldgm)
!
                if (nma .ne. nbno) then
                    vali(1) = nbno
                    vali(2) = nma
                    call u2mesg('F', 'MODELISA2_10', 1, nogp, 2,&
                                vali, 0, r8bid)
                endif
!
!
                do 22 in = 0, nma-1
!                 RECUPERE LE NOMBRE DE NOEUD DE LA MAILLE
                    call jelira(jexnum(noma//'.CONNEX', zi(ldgm+in)), 'LONMAX', nbnma, k8bid)
                    call jeveuo(jexnum(noma//'.CONNEX', zi(ldgm+in)), 'L', ldnm)
                    call jenuno(jexnum(mlgnma, zi(ldgm+in)), nommai)
!                 BOUCLE SUR LE NB DE NOEUD DE LA MAILLE
                    if (nbnma .ne. nbnoeu) then
                        call u2mesk('F', 'MODELISA_20', 1, nommai)
                    endif
                    do 25 inbn = 1, nbnma
                        inoe = zi(ldnm+inbn-1)
                        call jenuno(jexnum(mlgnno, inoe), nomnoe)
                        do 24 ino = 1, nbno
                            if (zk8(itbno+ino-1) .eq. nomnoe) then
                                zk8(itbmp+ino-1) = nommai
!
                                goto 22
                            endif
24                      continue
25                  continue
!                 SI ON PASSE ICI AUCUN DES NOEUDS DU DISCRET APPARTIENT
!                 A LA SURFACE, ET CE N'EST PAS NORMAL
                    write(ifm,*)'GROUP_MA :', (' '//zk24(jdls+ii-1),&
                    ii=1,ng)
                    call u2mesk('F', 'MODELISA_21', 1, nomnoe)
22              continue
!              PREPARATION DES IMPRESSIONS DANS LE FICHIER MESSAGE
                lorep = 5
                if (irep .eq. 1) lorep = 6
!
!
!            VERIF QU'UN DISCRET EST FIXE A CHACUN DES NOEUDS DU RADIER
!            (UNE SEULE FOIS PAR OCCURRENCE DE MASS_AJOU)
                if (nc .eq. 1) then
                    do 227 ino = 1, nbno
                        if (zk8(itbmp + ino - 1) .eq. ' ') then
                            call jenuno(jexnum(mlgnno, ino), nomnoe)
                            call u2mesk('F', 'MODELISA2_8', 1, nomnoe)
                        endif
227                  continue
                endif
!
                if (iunite .gt. 0 .and. niv .eq. 2) then
                    do 27 i = 1, nbno
                        iv = 1
                        jd = itbmp + i - 1
                        jn = itbno + i - 1
                        if (nbnoeu .eq. 1) then
                            if (transl) then
                                write(iunite,1011) 'MAILLE',zk8(jn),&
                                car(nc)(1:lokm), (zr(irgno+6*i-6+jj),&
                                jj=0,5), repdis(irep)(1:lorep)
                            endif
                        endif
27                  continue
                endif
!
                do 28 i = 1, nbno
                    iv = 1
                    jd = itbmp + i - 1
                    jn = itbno + i - 1
!
                    call affdis(ndim, irep, eta, car(nc), zr(irgno+6*i-6),&
                                jdc, jdv, ivr, iv, kma,&
                                ncmp, l, jdcinf, jdvinf, isym,&
                                ifm)
                    call nocart(cartdi, 3, ' ', 'NOM', 1,&
                                zk8(jd), 0, ' ', dimcar)
                    call nocart(cart(l), 3, ' ', 'NOM', 1,&
                                zk8(jd), 0, ' ', ncmp)
!                 AFFECTATION DE MATRICE RIGIDITE NULLE
                    iv = 1
!
                    call r8inir(nbval2, 0.0d0, vale, 1)
                    call affdis(ndim, irep, eta, lamass, vale,&
                                jdc, jdv, ivr, iv, kma,&
                                ncmp, l, jdcinf, jdvinf, isym,&
                                ifm)
                    call nocart(cartdi, 3, ' ', 'NOM', 1,&
                                zk8(jd), 0, ' ', dimcar)
                    call nocart(cart(l), 3, ' ', 'NOM', 1,&
                                zk8(jd), 0, ' ', ncmp)
28              continue
            endif
34      continue
!
30  end do
!
    call jedetr('&&TMPDISCRET')
    call jedetr('&&TMPTABNO')
    call jedetr('&&TMPRIGNO')
    call jedetr('&&TMPRIGTO')
    call jedetr('&&TMPTABMP')
    call getfac('RIGI_PARASOL', nborp)
    call getfac('RIGI_MISS_3D', nborm)
    if (nborp .eq. 0 .and. nborm .eq. 0) then
        do 240 i = 1, 3
            call jedetr(tmpnd(i))
            call jedetr(tmpvd(i))
240      continue
        call jedetr(tmcinf)
        call jedetr(tmvinf)
    endif
!
    call jedema()
!
    1000 format(/,&
     &    ' <DISCRET> MATRICES AFFECTEES AUX ELEMENTS DISCRET ',&
     &    '(REPERE ',a6,'), OCCURRENCE ',i4)
!
    1011 format(' _F(',a,'=''',A8,''', CARA=''',A,''',',/,&
     &       '   VALE=(',3(1x,1pe12.5,','),/,&
     &       '         ',3(1x,1pe12.5,','),'),',/,&
     &       '   REPERE=''',A,'''),')
end subroutine

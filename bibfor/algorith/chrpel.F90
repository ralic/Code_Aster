subroutine chrpel(champ1, repere, nbcmp, icham, type_cham,&
                  nomch, modele, carele)
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
! aslint: disable=W1501

    implicit none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/angvxy.h"
#include "asterfort/assach.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/cesvar.h"
#include "asterfort/chrgd.h"
#include "asterfort/chrpan.h"
#include "asterfort/copisd.h"
#include "asterfort/cylrep.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/manopg.h"
#include "asterfort/matrot.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/reliem.h"
#include "asterfort/sepach.h"
#include "asterfort/ut2vgl.h"
#include "asterfort/utmess.h"
#include "asterfort/utpsgl.h"
#include "asterfort/utpvgl.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/ddot.h"
    !
    integer :: nbcmp, icham
    character(len=*) :: champ1, repere, type_cham, nomch, modele, carele
! ----------------------------------------------------------------------
    !
!     BUT : CHANGEMENT DE REPERE DANS LE CAS D'UN CHAM_ELEM
! ----------------------------------------------------------------------
!     ARGUMENTS :
!     CHAMP1   IN  K16  : NOM DU CHAMP A TRAITER (CHAMP OUT)
!     REPERE   IN  K16  : TYPE DE REPERE (UTILISATEUR OU CYLINDRIQUE
!                         OU COQUE OU COQUE_UTIL_INTR OU COQUE_INTR_UTIL 
!                         OU COQUE_UTIL_CYL)
!     NBCMP    IN  I    : NOMBRE DE COMPOSANTES A TRAITER
!     ICHAM    IN  I    : NUMERO D'OCCURRENCE
!     type_cham     IN  K16  : TYPE DU CHAMP :'TENS' 'VECT' OU 'COQUE'
!     NOMCH    IN  K16  : NOM DE CHAMP
! ---------------------------------------------------------------------
    !
    integer :: i, ii, ino, iad, ipt, isp
    integer :: jcesd, jcesv, jcesl, nbpt,  ncmp
    integer ::  jconx2, nbsp, inel,  npain
    integer :: ibid, nbma,  iret, inbno
    integer :: ndim, nbm, idmail, nbmail, imai
    integer :: inoeu, iret0, iret1, nbgno, igno, nncp
    integer :: ierk, mnogav, iadr, ipaxe, ipaxe2
    integer :: nbno, nbpg, nuno, ipg
    integer :: type_pt
    integer, parameter :: type_unknown = 0, type_noeud = 1, type_gauss = 2
! nb max de points (noeuds|gauss) par élément
    integer, parameter :: nptmax = 30
    integer, dimension(6) :: permvec
    logical :: exi_cmp
    real(kind=8) :: valr, xnormr
    real(kind=8), dimension(3) :: xbary, angnot
    real(kind=8), dimension(3) :: orig, axez, vectx, vecty, angrep
    real(kind=8), dimension(3, 3) :: pgl, pgcyl, pgu
    real(kind=8), dimension(3, nptmax), target :: xno, xpg
    real(kind=8), dimension(:, :), pointer :: xpt => null()
    character(len=3) :: tsca
    character(len=8) :: ma, k8b, typmcl(2), nomgd, tych, param
    character(len=8) :: lpain(5), paout, licmp(9), nomgdr, paoutc
    character(len=16) :: option, motcle(2), nomch2
    character(len=19) :: chams1, chams0, ligrel, manoga, canbsp
    character(len=19) :: changl, carte, chr, chi, ch1, ch2
    character(len=24) :: mesmai, chgeom, lchin(5), chaout
    character(len=24) :: valk(3), chcara(18)
    character(len=8), pointer :: nom_cmp(:) => null()
    character(len=8), pointer :: cesk(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    integer, pointer :: connex(:) => null()
    !
    call jemarq()
    ipaxe = 0
    motcle(1) = 'GROUP_MA'
    typmcl(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(2) = 'MAILLE'
    canbsp = '&&CHRPEL.NBSP'
    !
    mesmai = '&&CHRPEL.MES_MAILLES'
    !
    if (nbcmp .gt. 0) then
        AS_ALLOCATE(vk8=nom_cmp, size=nbcmp)
        call getvtx('MODI_CHAM', 'NOM_CMP', iocc=icham, nbval=nbcmp, vect=nom_cmp,&
                    nbret=ibid)
    else
        call utmess('F', 'ALGORITH2_6')
    endif
    !
    call dismoi('NOM_LIGREL', champ1, 'CHAM_ELEM', repk=ligrel)
    !
    !
! ----- DEFINITION ET CREATION DU CHAM_ELEM SIMPLE CHAMS1
! ----- A PARTIR DU CHAM_ELEM CHAMP1
    !
    chams0='&&CHRPEL.CHAMS0'
    chams1='&&CHRPEL.CHAMS1'
    call celces(champ1, 'V', chams0)
    call cesred(chams0, 0, [0], nbcmp, nom_cmp,&
                'V', chams1)
    call detrsd('CHAM_ELEM_S', chams0)
    call jeveuo(chams1//'.CESK', 'L', vk8=cesk)
    call jeveuo(chams1//'.CESD', 'L', jcesd)
    ma = cesk(1)
    nomgd = cesk(2)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    !
!     ON EXCLUT LES MOT-CLES 'NOEUD' ET 'GROUP_NO'
    !
    call jeveuo(ma//'.DIME   ', 'L', inbno)
    call wkvect('&&CHRPEL.NOEUDS', 'V V K8', zi(inbno), inoeu)
    call getvtx('AFFE', 'NOEUD', iocc=icham, nbval=0, nbret=iret0)
    call jeexin(ma//'.GROUPENO', ierk)
    if (ierk .ne. 0) then
        call jelira(ma//'.GROUPENO', 'NMAXOC', nbgno)
        call wkvect('&&CHRPEL.GROUP_NO', 'V V K24', nbgno, igno)
        call getvtx('AFFE', 'GROUP_NO', iocc=icham, nbval=0, nbret=iret1)
    else
        iret1=0
    endif
    if (iret0 .lt. 0) then
        k8b='NOEUD   '
    else if (iret1.lt.0) then
        k8b='GROUP_NO'
    else
        goto 100
    endif
    valk (1) = k8b
    valk (2) = nomch
    valk (3) = ' '
    call utmess('F', 'ALGORITH12_42', nk=3, valk=valk)
100 continue
    call jedetr('&&CHRPEL.NOEUDS')
    call jedetr('&&CHRPEL.GROUP_NO')
! Nombre de mailles total dans le maillage
    nbma = zi(jcesd-1+1)
!   nombre de composantes du champ simple chams1 
    ncmp = zi(jcesd-1+2)
!   comme chams1 a été créé à partir des composantes sélectionnées par 
!   l'utilisateur, on doit avoir:  
    ASSERT( ncmp == nbcmp ) 
!   Détermination de la dimension à partir du maillage  
    ndim = 3
    call dismoi('Z_CST', ma, 'MAILLAGE', repk=k8b)
    if (k8b .eq. 'OUI') ndim = 2
    !
! Construction de la liste des numéros de mailles 
! sélectionnées par les mots-clés GROUP_MA et MAILLE 
    call reliem(' ', ma, 'NU_MAILLE', 'AFFE', icham,&
                2, motcle, typmcl, mesmai, nbm)
    if (nbm .gt. 0) then
        nbmail = nbm
        call jeveuo(mesmai, 'L', idmail)
    else
        nbmail = nbma
    endif
    !
    call jeexin(ma//'.CONNEX', iret)
    ASSERT(iret.ne.0)
    call jeveuo(ma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    call jeveuo(chams1//'.CESV', 'E', jcesv)
    call jeveuo(chams1//'.CESL', 'L', jcesl)
    !
    do i = 1, 3
        axez(i) = 0.0d0
        orig(i) = 0.0d0
        angnot(i) = 0.0d0
    end do
    !
!**
!*** Changement de repère "UTILISATEUR"
!**
    !
    if (repere(1:11) .eq. 'UTILISATEUR') then
        !
!        SI LE NOUVEAU REPERE EST DONNE VIA DES VECTEURS
        call getvr8('AFFE', 'VECT_X', iocc=1, nbval=3, vect=vectx,&
                    nbret=ibid)
        if (ibid .ne. 0) then
            call getvr8('AFFE', 'VECT_Y', iocc=1, nbval=3, vect=vecty,&
                        nbret=ibid)
            if (ndim .ne. 3) then
                call utmess('F', 'ALGORITH2_4')
            endif
            call angvxy(vectx, vecty, angnot)
        else
            if (ndim .eq. 3) then
                call getvr8('AFFE', 'ANGL_NAUT', iocc=1, nbval=3, vect=angnot,&
                            nbret=ibid)
                if (ibid .ne. 3) then
                    call utmess('F', 'ALGORITH2_7')
                endif
            else
                call getvr8('AFFE', 'ANGL_NAUT', iocc=1, scal=angnot(1), nbret=ibid)
                if (ibid .ne. 1) then
                    valr = angnot(1)
                    call utmess('A', 'ALGORITH12_43', sr=valr)
                endif
            endif
            angnot(:) = angnot(:)*r8dgrd()
        endif
        !
! Calcul de pgu = matrice de passage du repère par défaut (repère global du maillage) 
! vers le repère utilisateur  
        call matrot(angnot, pgl)
! matrot retourne la transposée de la matrice de passage : on transpose pour avoir 
! la matrice de passage 
        pgu=transpose(pgl) 
        !
! Appliquer le changement de repère pour les mailles sélectionnées 
        !
        do inel = 1, nbmail
            if (nbm .ne. 0) then
                imai = zi(idmail+inel-1)
            else
                imai = inel
            endif
            nbpt = zi(jcesd-1+5+4* (imai-1)+1)
            nbsp = zi(jcesd-1+5+4* (imai-1)+2)
            do ipt = 1, nbpt
                do isp = 1, nbsp
                    exi_cmp = .false.
                    do ii = 1, nbcmp
                        call cesexi('S', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            exi_cmp = .true.
                        endif
                    end do 
                    if (exi_cmp) then
                        call chrgd(nbcmp, jcesd, jcesl, jcesv, imai,&
                                   ipt, isp, type_cham, tsca, pgu)
                    else
                        goto 10
                    endif
                enddo
            enddo
 10         continue
        enddo
! Champ simple -> Champ
        call dismoi('NOM_OPTION', champ1, 'CHAM_ELEM', repk=option)
        call cescel(chams1, ligrel, option, ' ', 'OUI',&
                    nncp, 'G', champ1, 'F', ibid)
        call detrsd('CHAM_ELEM_S', chams1)
        !
!**
!*** Changement de repère "CYLINDRIQUE"
!**
    else if (repere(1:11).eq.'CYLINDRIQUE') then
        !
        call dismoi('TYPE_CHAMP', champ1, 'CHAMP', repk=tych, arret='C',&
                    ier=iret)
        if (ndim .eq. 3) then
            call getvr8('AFFE', 'ORIGINE', iocc=1, nbval=3, vect=orig,&
                        nbret=ibid)
            if (ibid .ne. 3) then
                call utmess('F', 'ALGORITH2_8')
            endif
            call getvr8('AFFE', 'AXE_Z', iocc=1, nbval=3, vect=axez,&
                        nbret=ibid)
            if (ibid .eq. 0) then
                call utmess('F', 'ALGORITH2_9')
            endif
        else
            call getvr8('AFFE', 'ORIGINE', iocc=1, nbval=2, vect=orig,&
                        nbret=ibid)
            if (ibid .ne. 2) then
                call utmess('A', 'ALGORITH2_10')
            endif
            call getvr8('AFFE', 'AXE_Z', iocc=1, nbval=0, nbret=ibid)
            if (ibid .ne. 0) then
                call utmess('A', 'ALGORITH2_11')
            endif
            axez(1) = 0.0d0
            axez(2) = 0.0d0
            axez(3) = 1.0d0
        endif
        xnormr = 0.0d0
        call normev(axez, xnormr)
        call jeveuo(ma//'.COORDO    .VALE', 'L', vr=vale)
        !
        manoga='&&CHRPEL.MANOGA'
        !
        if (nomch(1:4) .eq. 'SIEF' .or. nomch(1:4) .eq. 'SIGM') then
            param='PCONTRR'
        else if (nomch(1:2).eq.'EP') then
            param='PDEFOPG'
        else if (nomch(1:4).eq.'VARI') then
            param='PVARIGR'
        else
            call utmess('F', 'ALGORITH2_14', sk=nomch)
        endif
        !
        nomch2 = nomch
        call manopg(ligrel, nomch2, param, manoga)
        !
! Permutation des composantes en dimension 2
        !
! Initialisation à l'identité 
        permvec(:)=(/(i,i=1,6)/)
        if (ndim == 2) then
            select case (type_cham(1:4))
            case('TENS')
            permvec(4) = 5
            case('VECT')
            permvec(2) = 3
            permvec(3) = 2
        end select
        endif
        !
! Localisation du champ : noeuds/pts de Gauss
        type_pt = type_unknown
        if (tych(1:4) == 'VECT') then
            type_pt = type_noeud
        endif 
        if (tych(1:4) == 'ELNO') then
            type_pt = type_noeud
        else if (tych(1:4) == 'ELGA') then
            type_pt = type_gauss
        endif 
        !
        ASSERT(type_pt /= type_unknown ) 
        !
        call jeveuo(manoga//'.CESV', 'L', mnogav)
        !
        do inel = 1, nbmail
            if (nbm .ne. 0) then
                imai = zi(idmail+inel-1)
            else
                imai = inel
            endif
! Nombre de noeuds de la maille courante 
            nbno = zi(jcesd-1+5+4* (imai-1)+1)
! Nombre de sous-points 
            nbsp = zi(jcesd-1+5+4* (imai-1)+2)
! Nombre de composantes du champ à transformer 
            nbcmp = zi(jcesd-1+5+4* (imai-1)+3)
            !
! Coordonnées des noeuds de la maille courante
! 
            xno(:,:) = 0.d0
            do ino = 1, nbno
                nuno = connex(zi(jconx2+imai-1)+ino-1)
                xno(1,ino) = vale(1+3*(nuno-1)-1+1)
                xno(2,ino) = vale(1+3*(nuno-1)-1+2)
                if (ndim == 3) then
                    xno(3,ino) = vale(1+3*(nuno-1)-1+3)
                endif 
            end do
            !
            select case (type_pt)
            case (type_noeud)
! On se place aux noeuds de la maille
                nbpt=nbno
                xpt => xno(:,:)
            case (type_gauss)
! On se place aux points de Gauss de la maille 
                nbpg = zi(jcesd-1+5+4* (imai-1)+1)
                !
!  Coordonnées des points de Gauss de la maille courante 
                !
                do ipg = 1, nbpg
                    xpg(:,ipg) = 0.d0
                    iadr=mnogav-1+iad+1+nbno*(ipg-1)
                    do ino = 1, nbno
                        xpg(:,ipg) = xpg(:,ipg) + xno(:,ino)*zr(iadr+ino)
                    end do
                end do
                nbpt = nbpg
                xpt => xpg(:,:) 
                !
            case default
                ASSERT(.false.) 
            end select
            !
            do ipt = 1, nbpt
! Calcul de la matrice de passage vers le repère cylindrique 
                call cylrep(ndim, xpt(:, ipt), axez, orig, pgcyl,&
                            ipaxe)
!   
! Si le point x appartient à l'axe du repère cylindrique 
                if (ipaxe > 0) then
                    call jenuno(jexnum(ma//'.NOMNOE', ino), k8b)
                    call utmess('A', 'ALGORITH2_13')
! on calcule la matrice de passage au centre de gravité de l'élément
                    xbary(:)=sum(xno(:,1:nbno), dim=2)
                    xbary(:) = xbary(:)/nbno
                    ipaxe2 = 0
                    call cylrep(ndim, xbary, axez, orig, pgcyl,&
                                ipaxe2)
! et si le centre de gravité de l'élément est aussi sur l'axe, on s'arrête
                    if (ipaxe2 > 0) then
                        call utmess('F', 'ALGORITH2_13')
                    endif
                endif 
! Boucle sur les sous-points
                do isp = 1, nbsp
                    exi_cmp = .true.
                    do ii = 1, nbcmp
! la composante ii du champ existe-t-elle? 
                        exi_cmp = .false.
                        call cesexi('S', jcesd, jcesl, imai, ipt,&
                                    isp, ii, iad)
                        if (iad .gt. 0) then
                            exi_cmp = .true.
                        endif
                    end do 
! si oui,  
                    if (exi_cmp) then
                        !
! on applique le changement de base 
                        !
                        call chrgd(nbcmp, jcesd, jcesl, jcesv, imai,&
                                   ipt, isp, type_cham, tsca, pgcyl,&
                                   permvec)
! sinon on ne fait rien 
                    else
                        goto 20
                    endif
                end do 
            end do 
 20         continue 
        end do 
        !
        call dismoi('NOM_OPTION', champ1, 'CHAM_ELEM', repk=option)
        call cescel(chams1, ligrel, option, ' ', 'OUI',&
                    nncp, 'G', champ1, 'F', ibid)
        call detrsd('CHAM_ELEM_S', chams1)
        if (ipaxe .ne. 0) then
            call utmess('A', 'ALGORITH17_22', si=ipaxe)
        endif
        !
        else if((repere(1:5) .eq.'COQUE') .or. (repere(1:15).eq.'COQUE_INTR_UTIL').or. &
       (repere(1:15).eq.'COQUE_UTIL_INTR').or.(repere(1:14).eq.'COQUE_UTIL_CYL')) &
       then
        !
        call megeom(modele, chgeom)
        call mecara(carele, chcara)
        !
        if (( type_cham(1:10) .eq. 'COQUE_GENE' ) .and. ( repere(1:14).eq.'COQUE_UTIL_CYL')) then
            call utmess('F', 'ELEMENTS5_55', nk=2, valk=(/'COQUE_UTIL_CYL', 'COQUE_GENE    '/))
        endif 
        if (type_cham(1:10) .eq. 'COQUE_GENE') then
            option = 'REPE_GENE'
! Nb de paramètres en entrée de l'option  
            npain = 4
        else if (type_cham(1:7).eq.'TENS_3D') then
            option = 'REPE_TENS'
            npain = 5
        else
            call utmess('F', 'ELEMENTS5_53', sk=type_cham)
        endif
        !
!    GENERATION D UN CHAMP D'ANGLES (CARTE CONSTANTE)
        !
        carte = '&&CHRPEL.ANGL_REP'
        angrep(:) = 0.0d0
        !
        if (repere .eq. 'COQUE_INTR_UTIL') then
            angrep(3)=1.d0
        else if (repere.eq.'COQUE_UTIL_INTR') then
            angrep(3)=2.d0
        else if (repere.eq.'COQUE_UTIL_CYL') then
            angrep(3)=3.d0
        endif
        licmp(1) = 'ALPHA'
        licmp(2) = 'BETA'
        licmp(3) = 'REP'
        licmp(4) = 'AXE_X'
        licmp(5) = 'AXE_Y'
        licmp(6) = 'AXE_Z'
        licmp(7) = 'O_X'
        licmp(8) = 'O_Y'
        licmp(9) = 'O_Z'
        call mecact('V', carte, 'MODELE', modele, 'CAORIE',&
                    ncmp=9, lnomcmp=licmp, vr=angrep)
        !
!  CREATION D UN CHAM_ELEM D'ANGLES EN LISANT LES ANGL_REP
! 
        changl = '&&CHRPEL.ANGL'
        call chrpan(modele, carte, option, changl)
        !
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PCACOQU'
        lchin(2) = chcara(7)
        lpain(3) = 'PANGREP'
        lchin(3) = changl
        lchin(4) = champ1
        lpain(5) = 'PNBSP_I'
        lchin(5) = chcara(16)
!  
        call dismoi('NOM_GD', lchin(4), 'CHAMP', repk=nomgdr)
        !
        if (type_cham .eq. 'COQUE_GENE') then
            if (nomch .eq. 'EFGE_ELGA') then
                lpain(4) = 'PEFGAIN'
                paout = 'PEFGAOUT'
                if (nomgdr(5:6) .eq. '_C') paoutc = 'PEFGAOUC'
            else if (nomch.eq.'EFGE_ELNO') then
                lpain(4) = 'PEFNOIN'
                paout = 'PEFNOOUT'
                if (nomgdr(5:6) .eq. '_C') paoutc = 'PEFNOOUC'
            else if (nomch.eq.'DEGE_ELGA') then
                lpain(4) = 'PDGGAIN'
                paout = 'PDGGAOUT'
                if (nomgdr(5:6) .eq. '_C') paoutc = 'PDGGAOUC'
            else if (nomch.eq.'DEGE_ELNO') then
                lpain(4) = 'PDGNOIN'
                paout = 'PDGNOOUT'
                if (nomgdr(5:6) .eq. '_C') paoutc = 'PDGNOOUC'
            else if (nomch.eq.'SIEF_ELGA') then
                lpain(4) = 'PEFGAIN'
                paout = 'PEFGAOUT'
                if (nomgdr(5:6) .eq. '_C') paoutc = 'PEFGAOUC'
            else
                call utmess('F', 'ELEMENTS5_51', sk=nomch)
            endif
        else if (type_cham.eq.'TENS_3D') then
            if (nomch .eq. 'SIGM_ELGA') then
                lpain(4) = 'PCOGAIN'
                paout = 'PCOGAOUT'
            else if (nomch.eq.'SIGM_ELNO') then
                lpain(4) = 'PCONOIN'
                paout = 'PCONOOUT'
            else if (nomch.eq.'EPSI_ELGA') then
                lpain(4) = 'PDEGAIN'
                paout = 'PDEGAOUT'
            else if (nomch.eq.'EPSI_ELNO') then
                lpain(4) = 'PDENOIN'
                paout = 'PDENOOUT'
            else
                call utmess('F', 'ELEMENTS5_52', sk=nomch)
            endif
            !
        endif
        call exisd('CHAM_ELEM_S', canbsp, iret1)
        if (iret1 .ne. 1) then
            call dismoi('MXNBSP', champ1(1:19), 'CHAM_ELEM', repi=nbsp)
            !
! SI LE CHAMP A DEJA ETE EXTRAIT IL FAUT APPELER CESVAR AVEC CE CHAMP
            !
            if (nbsp .eq. 1) then
                call cesvar(champ1(1:19), ' ', ligrel, canbsp)
            else
                call cesvar(carele, ' ', ligrel, canbsp)
            endif
        endif
        chaout = chams1
        call copisd('CHAM_ELEM_S', 'V', canbsp, chaout)
        !
        if (nomgdr(5:6) .eq. '_C') then
            chr='&&CHRPEL.CHR'
            chi='&&CHRPEL.CHI'
            ch1='&&CHRPEL.CH1'
            ch2='&&CHRPEL.CH2'
            call sepach(carele, lchin(4), 'V', chr, chi)
            lchin(4)=chr
            call calcul('S', option, ligrel, npain, lchin(1:npain),&
                        lpain(1:npain), 1, ch1, paout, 'V',&
                        'OUI')
            lchin(4)=chi
            call calcul('S', option, ligrel, npain, lchin(1:npain),&
                        lpain(1:npain), 1, ch2, paout, 'V',&
                        'OUI')
            call assach(ch1, ch2, 'V', chaout, parout=paoutc)
            call detrsd('CHAMP', chr)
            call detrsd('CHAMP', chi)
            call detrsd('CHAMP', ch1)
            call detrsd('CHAMP', ch2)
        else
            call calcul('S', option, ligrel, npain, lchin(1:npain),&
                        lpain(1:npain), 1, chaout, paout, 'V',&
                        'OUI')
        endif
        call detrsd('CHAM_ELEM_S', chaout)
        call copisd('CHAMP_GD', 'G', chaout, champ1)
    endif
    AS_DEALLOCATE(vk8=nom_cmp)
    call exisd('CHAM_ELEM_S', canbsp, iret1)
    if (iret1 .ne. 0) call detrsd('CHAM_ELEM_S', canbsp)
    call jeexin(mesmai, iret)
    if (iret .ne. 0) call jedetr(mesmai)
    call jedema()
    !
end subroutine chrpel

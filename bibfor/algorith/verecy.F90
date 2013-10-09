subroutine verecy(intf, numd, numg, nbsec, prec,&
                  distrf)
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
!***********************************************************************
!    P. RICHARD     DATE 13/12/91
!-----------------------------------------------------------------------
!  BUT:       < VERIFICATION REPETITIVITE CYCLIQUE>
! aslint: disable=
    implicit none
!
!  VERIFICATION DE LA REPETITIVITE CYCLIQUE SUR LE MAILLAGE ET LA
!  DEFINITION DES INTERFACES
!
!-----------------------------------------------------------------------
!
! INTF     /I/: NOM UTILISATEUR DE L'INTERF_DYNA
! NUMD     /I/: NUMERO DE L'INTERFACE DE DROITE
! NUMG     /I/: NUMERO DE L'INTERFACE DE GAUCHE
! NBSEC    /I/: NOMBRE DE SECTEUR
! PREC     /R/: PRECISION DE RECHERCHE DE PROXIMITE
! DISTRF   /R/: DISTANCE DE REFERENCE
!
!
#include "jeveux.h"
#include "asterfort/bmnoin.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: vali(2)
!
!
!
    character(len=6) :: pgc
    character(len=24) :: valk(3)
    character(len=8) :: intf, kbid, mailla, nomnod, nomnog, nomnj
    character(len=50) :: diag
    logical :: ordre
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, j, jnode, llcoo, llintg
    integer :: llista, llistb, ltnd, ltng, nbd, nbg, nbpbax
    integer :: nbpbr, nbpbse, nbpbto, nbpbvt, nbsec, numd, numg
    integer :: nunod, nunog
    real(kind=8) :: crit, difr, difz, dist, distj, distr, distrf
    real(kind=8) :: distrj, distz, distzj, pi, prec, pvdif, rd
    real(kind=8) :: rg, teta, xd, xg, yd, yg, zd
    real(kind=8) :: zg, zpv, zpvref
!-----------------------------------------------------------------------
    data pgc /'VERECY'/
!-----------------------------------------------------------------------
!
    distrj =0.d0
    distzj =0.d0
!
    call jemarq()
    pi=4.d0*atan(1.d0)
!
!--------VERIFICATION NOMBRE DE NOEUDS INTERFACES DROITE ET GAUCHE------
!
    kbid=' '
    call bmnoin(' ', intf, kbid, numd, 0,&
                [0], nbd)
    kbid=' '
    call bmnoin(' ', intf, kbid, numg, 0,&
                [0], nbg)
!
!
    if (nbg .ne. nbd) then
        vali (1) = nbd
        vali (2) = nbg
        call utmess('E', 'ALGORITH16_50', ni=2, vali=vali)
    endif
!
!
!--------------------VERIFICATION REPETITIVITE GEOMETRIQUE--------------
!
!
    call dismoi('NOM_MAILLA', intf, 'INTERF_DYNA', repk=mailla)
!
!
!
    call wkvect('&&'//pgc//'.NOEUD.DROITE', 'V V I', nbd, ltnd)
    call wkvect('&&'//pgc//'.NOEUD.GAUCHE', 'V V I', nbg, ltng)
!
    kbid=' '
    call bmnoin(' ', intf, kbid, numd, nbd,&
                zi(ltnd), ibid)
    kbid=' '
    call bmnoin(' ', intf, kbid, numg, nbg,&
                zi(ltng), ibid)
!
    call jeveuo(mailla//'.COORDO    .VALE', 'L', llcoo)
!
    teta=2.d0*pi/nbsec
!
!     --- CONSTITUTION DE LISTA ET LISTB :
!         LE IEME NOEUD DE L'INTERFACE DROITE A POUR VIS-A-VIS
!         LE ZI(LISTA-1+I) EME NOEUD DE L'INTERFACE GAUCHE
!         RECIPROQUEMENT LE NOEUD DE POSITION J DE L'INTERFACE GAUCHE
!         EST LE VIS-A-VIS DU NOEUD DE POSITION ZI(LISTB-1+J) DE
!         L'INTERFACE DROITE.
    call wkvect('&&'//pgc//'.LISTA', 'V V I', nbd, llista)
    call wkvect('&&'//pgc//'.LISTB', 'V V I', nbd, llistb)
    nbpbax=0
    nbpbr=0
    nbpbse=0
    nbpbvt=0
    ordre = .true.
    do i = 1, nbd
!     --- BOUCLE SUR LES NOEUDS DE L'INTERFACE DROITE ---
        nunod=zi(ltnd+i-1)
        call jenuno(jexnum(mailla//'.NOMNOE', nunod), nomnod)
!
        xd=zr(llcoo+3*(nunod-1))
        yd=zr(llcoo+3*(nunod-1)+1)
        zd=zr(llcoo+3*(nunod-1)+2)
        rd = sqrt(xd*xd+yd*yd)
!
!       RECHERCHE DU NOEUD J (GAUCHE) LE PLUS PROCHE DE I (DROITE)
        do j = 1, nbd
!       --- BOUCLE SUR LES NOEUDS DE L'INTERFACE GAUCHE ---
            nunog=zi(ltng+j-1)
            call jenuno(jexnum(mailla//'.NOMNOE', nunog), nomnog)
            xg=zr(llcoo+3*(nunog-1))
            yg=zr(llcoo+3*(nunog-1)+1)
            zg=zr(llcoo+3*(nunog-1)+2)
            rg = sqrt(xg*xg+yg*yg)
            distr = abs(rd-rg)
            distz = abs(zd-zg)
            if (j .eq. 1 .or. (distr.le.distrj .and. distz.le.distzj)) then
!          --- CRITERE : RAYON ET HAUTEUR Z LES PLUS PROCHES ---
                distrj = distr
                distzj = distz
                distj = sqrt(distr*distr+distz*distz)
                jnode = j
                nomnj = nomnog
            else if (distr.le.distrj .or. distz.le.distzj) then
!          --- SI UN SEUL CRITERE EST BON, ON COMPARE LES DISTANCES ---
                dist = sqrt(distr*distr+distz*distz)
                if (dist .lt. distj) then
!
                    distrj = distr
                    distzj = distz
                    distj = dist
                    jnode = j
                    nomnj = nomnog
                endif
            endif
        end do
        zi(llista-1+i) = jnode
        if (zi(llistb-1+jnode) .ne. 0) then
!       --- CAS OU JNODE EST DEJA UN VIS-A-VIS ---
            nunog=zi(ltng+zi(llistb-1+jnode)-1)
            call jenuno(jexnum(mailla//'.NOMNOE', nunog), nomnog)
            valk (1) = nomnj
            valk (2) = nomnod
            valk (3) = nomnog
            call utmess('F', 'ALGORITH16_51', nk=3, valk=valk)
        endif
        zi(llistb-1+jnode) = i
!       SI JNODE EST DIFFERENT DE I, C'EST QUE LES NOEUDS D'INTERFACE
!       ONT ETE DONNES DANS UN ORDRE DE NON CORRESPONDANCE
        if (jnode .ne. i) ordre = .false.
        nunog=zi(ltng+jnode-1)
        call jenuno(jexnum(mailla//'.NOMNOE', nunog), nomnog)
        xg=zr(llcoo+3*(nunog-1))
        yg=zr(llcoo+3*(nunog-1)+1)
        zg=zr(llcoo+3*(nunog-1)+2)
!
! VERIFICATION OZ AXE REPETITIVITE
!
        difz=abs(zd-zg)
        if (distrf .lt. 0.d0) then
!       --- DISTANCE DE REFERENCE NON CONNUE
            crit=prec*1.d-2*max(abs(zd),abs(zg))
        else
            crit=prec*distrf
        endif
        if (difz .gt. crit) then
            nbpbax=nbpbax+1
            vali (1) = i
            valk (1) = nomnod
            valk (2) = nomnog
            call utmess('E', 'ALGORITH16_52', nk=2, valk=valk, si=vali(1))
        endif
!
!      VERIFICATION RAYON
!
        rd=((xd**2)+(yd**2))**0.5d0
        rg=((xg**2)+(yg**2))**0.5d0
!
        difr=abs(rd-rg)
        crit=prec*distrf
        if (distrf .lt. 0.d0) then
!       --- DISTANCE DE REFERENCE NON CONNUE
            crit=prec*1.d-2*max(rd,rg)
        else
            crit=prec*distrf
        endif
        if (difr .gt. crit) then
            nbpbr=nbpbr+1
            vali (1) = i
            valk (1) = nomnod
            valk (2) = nomnog
            call utmess('E', 'ALGORITH16_53', nk=2, valk=valk, si=vali(1))
        endif
!
!  VERIFICATION SENS ANGLE
!
        zpv=(xd*yg)-(yd*xg)
        if (zpv .lt. 0.d0) then
            nbpbse=nbpbse+1
            vali (1) = i
            valk (1) = nomnod
            valk (2) = nomnog
            call utmess('E', 'ALGORITH16_54', nk=2, valk=valk, si=vali(1))
        endif
!
! VERIFICATION VALEUR ANGLE
!
        zpvref=(sin(teta)*rd*rg)
        pvdif=abs(zpvref-abs(zpv))
        crit=zpvref*prec
        if (pvdif .gt. crit) then
            nbpbvt=nbpbvt+1
            vali (1) = i
            valk (1) = nomnod
            valk (2) = nomnog
            call utmess('E', 'ALGORITH16_55', nk=2, valk=valk, si=vali(1))
        endif
!
    end do
!
!
    nbpbto=nbpbax+nbpbr+nbpbse+nbpbvt
!
    if (nbpbto .eq. 0) then
        call utmess('I', 'ALGORITH16_56')
        diag = ' '
    else if (nbpbax.eq.nbd) then
        diag=' AXE DE REPETITIVITE DIFFERENT DE 0Z      '
    else if (nbpbvt.eq.nbd) then
        diag='NOMBRE DE SECTEURS DONNE ERRONE          '
    else if (nbpbse.eq.nbd) then
        diag='INVERSION INTERFACE DROITE ET GAUCHE'
    else if (nbpbr.eq.nbpbto) then
        diag='INTERFACES DROITE ET GAUCHE NON COMPATIBLES'
    else
        diag=' PAS DE DIAGNOSTIC SIMPLE TROUVE'
    endif
!
    if (.not.ordre .and. diag .eq. ' ') then
!     --- LES NOEUDS NE SONT PAS EN VIS-A-VIS ---
!         ON REGARDE D'ABORD SI LE TRI EST PLAUSIBLE
        do i = 1, nbd
            if (zi(llistb-1+zi(llista-1+i)) .ne. i) then
                diag = 'TRI DES NOEUDS IMPOSSIBLE'
                goto 40
            endif
        end do
 40     continue
!
        call utmess('A', 'ALGORITH16_57')
        call jeveuo(jexnum(intf//'.IDC_LINO', numg), 'E', llintg)
!    --- ON ORDONNE LES NOEUDS DE LLINTG SUIVANT LLISTA
        do i = 1, nbd
!        --- RECOPIE DE LLINT2 DANS LLISTB
            zi(llistb-1+i) = zi(llintg-1+i)
        end do
        do i = 1, nbd
            zi(llintg-1+i) = zi(llistb-1+zi(llista-1+i))
        end do
!
    endif
!
!     --- DESTRUCTION OBJETS SUR VOLATILE
    call jedetr('&&VERECY.LISTA')
    call jedetr('&&VERECY.LISTB')
    call jedetr('&&VERECY.NOEUD.DROITE')
    call jedetr('&&VERECY.NOEUD.GAUCHE')
!
    if (diag .ne. ' ') then
        valk (1) = diag
        call utmess('F', 'ALGORITH16_58', sk=valk(1))
    endif
!
    call jedema()
end subroutine

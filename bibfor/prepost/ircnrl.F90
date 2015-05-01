subroutine ircnrl(ifi, nbno, prno, nueq, nec,&
                  dg, ncmpmx, vale, nomcmp, nomnoe,&
                  lcor, ndim, coor, numnoe, nbcmpt,&
                  nucmpu, lsup, borsup, linf, borinf,&
                  lmax, lmin, formr)
! aslint: disable=W1504
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/wkvect.h"
    integer :: ifi, nbno, prno(*), nueq(*), nec, dg(*), ncmpmx
    integer :: ndim, numnoe(*), nbcmpt, nucmpu(*)
    real(kind=8) :: borsup, borinf, coor(*), vale(*)
    character(len=*) :: nomcmp(*), nomnoe(*), formr
    aster_logical :: lcor, lsup, linf, lmax, lmin
!
!----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!        ECRITURE D'UN CHAM_NO SUR FICHIER IFI AU FORMAT 'RESULTAT'
!        A VALEURS REELLES
!      ENTREE:
!         IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
!         NBNO  : NOMBRE DE NOEUDS A IMPRIMER
!         PRNO  : OBJET .PRNO(ILIGREL) D'UN PROF_CHNO
!         NUEQ  : OBJET .NUEQ D'UN PROF_CHNO
!         NEC   : NOMBRE D'ENTIERS-CODES
!         DG    : TABLEAU DES ENTIERS CODES
!         NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
!         VALE  : VALEURS DU CHAM_NO
!         NOMCMP: NOMS DES CMP
!         NOMNOE: NOMS DES NOEUDS
!         LCOR  : IMPRESSION DES COORDONNEES .TRUE. IMPRESSION
!         NDIM  : DIMENSION DU MAILLAGE
!         COOR  : COORDONNEES D'UN MAILLAGE
!         NUMNOE: NUMEROS DES NOEUDS A IMPRIMER
!         NBCMPT: NOMBRE DE COMPOSANTES A IMPRIMER
!         NOCMPU: NUMEROS DES COMPOSANTES A IMPRIMER
!         LSUP  : =.TRUE. INDIQUE PRESENCE D'UNE BORNE SUPERIEURE
!         BORSUP: VALEUR DE LA BORNE SUPERIEURE
!         LINF  : =.TRUE. INDIQUE PRESENCE D'UNE BORNE INFERIEURE
!         BORINF: VALEUR DE LA BORNE INFERIEURE
!         LMAX  : =.TRUE. INDIQUE IMPRESSION VALEUR MAXIMALE
!         LMIN  : =.TRUE. INDIQUE IMPRESSION VALEUR MINIMALE
!         FORMR : FORMAT D'ECRITURE DES REELS SUR "RESULTAT"
!     ------------------------------------------------------------------
!     ATTENTION EN CAS DE MODIFICATION DE CE SS-PGME, PENSER A IRCNC8
!     ------------------------------------------------------------------
!
    real(kind=8) :: rundf
    integer :: impre
    character(len=8) :: nomcor(3), forcmp
    character(len=10) :: format
    character(len=50) :: fmt, form1
!
!-----------------------------------------------------------------------
    integer :: i, icm, icmp, icmp2, icomp2, icompt, id
    integer :: iec, ieq, if, ilign, imax, imin, inec
    integer :: inmax, inmin, inno, ino, ipos, ipres, irest
    integer :: irval, iva, ival, ivmax, ivmin, lgr
    integer :: ncmp
!-----------------------------------------------------------------------
    call jemarq()
    rundf = r8vide()
    nomcor(1) = 'X'
    nomcor(2) = 'Y'
    nomcor(3) = 'Z'
    format = formr
    lgr = lxlgut( format )
    id = 0
    if = 0
    do 2 i = 1, lgr-1
        if (format(i:i) .eq. 'D' .or. format(i:i) .eq. 'E' .or. format(i:i) .eq. 'F' .or.&
            format(i:i) .eq. 'G') then
            id = i+1
            goto 2
        endif
        if (format(i:i) .eq. '.') then
            if = i-1
            goto 2
        endif
  2 end do
    if (id .ne. 0 .and. if .ge. id) then
        forcmp = 'A'//format(id:if)
    else
        forcmp = 'A12'
    endif
!
! -- ALLOCATION DES TABLEAUX DE TRAVAIL ---
!
    call jedetr('&&IRCNRL.VAL')
    call wkvect('&&IRCNRL.VAL', 'V V R', ncmpmx, irval)
    call jedetr('&&IRCNRL.POS')
    call wkvect('&&IRCNRL.POS', 'V V I', ncmpmx, ipos)
    if (nec .gt. 0) then
        call jedetr('&&IRCNRL.ENT')
        call wkvect('&&IRCNRL.ENT', 'V V I', nec, inec)
        do 16 iec = 1, nec
            zi(inec-1+iec)=0
 16     continue
    endif
    if (lmax) then
        call jedetr('&&IRCNRL.MAX')
        call wkvect('&&IRCNRL.MAX', 'V V R', ncmpmx, imax)
        call jedetr('&&IRCNRL.NOEMAX')
        call wkvect('&&IRCNRL.NOEMAX', 'V V K8', ncmpmx, inmax)
        call jedetr('&&IRCNRL.NBVMAX')
        call wkvect('&&IRCNRL.NBVMAX', 'V V I', ncmpmx, ivmax)
        do 70 i = 1, ncmpmx
            zr(imax-1+i)=rundf
 70     continue
    endif
    if (lmin) then
        call jedetr('&&IRCNRL.MIN')
        call wkvect('&&IRCNRL.MIN', 'V V R', ncmpmx, imin)
        call jedetr('&&IRCNRL.NOEMIN')
        call wkvect('&&IRCNRL.NOEMIN', 'V V K8', ncmpmx, inmin)
        call jedetr('&&IRCNRL.NBVMIN')
        call wkvect('&&IRCNRL.NBVMIN', 'V V I', ncmpmx, ivmin)
        do 71 i = 1, ncmpmx
            zr(imin-1+i)=rundf
 71     continue
    endif
!
    do 11 inno = 1, nbno
        ino = numnoe(inno)
        do 17 iec = 1, nec
            dg(iec)= prno((ino-1)*(nec+2)+2+iec)
 17     continue
!
!        NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
!        IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
!
        ival = prno((ino-1)* (nec+2)+1)
        ncmp = prno((ino-1)* (nec+2)+2)
        if (ncmp .eq. 0) goto 11
!
        do 21 i = 1, ncmpmx
            zi(ipos-1+i) = 0
 21     continue
        icompt = 0
        impre = 0
        ipres = 0
        do 12 icmp = 1, ncmpmx
            if (exisdg(dg,icmp)) then
                ipres = ipres + 1
                ieq = nueq(ival-1+ipres)
                if (nbcmpt .ne. 0) then
                    do 13 icm = 1, nbcmpt
                        icmp2=nucmpu(icm)
                        if (icmp .eq. icmp2) then
                            zr(irval-1+icm) = vale(ieq)
                            zi(ipos-1+icm) = icmp
                            goto 12
                        endif
 13                 continue
                else
                    icompt=ipres
                    zr(irval-1+icompt) = vale(ieq)
                    zi(ipos-1+icompt) = icmp
                endif
            endif
 12     continue
!
! --- RETASSAGE POUR IMPRIMER COMPOSANTES ORDRE UTILISATEUR---
!
        if (nbcmpt .ne. 0) then
            icompt=0
            do 14 i = 1, nbcmpt
                if (zi(ipos-1+i) .ne. 0) then
                    icompt=icompt+1
                    zi(ipos-1+icompt)=zi(ipos-1+i)
                    zr(irval-1+icompt)=zr(irval-1+i)
                endif
 14         continue
        endif
        do 15 iec = 1, nec
            if (dg(iec) .ne. zi(inec-1+iec)) then
                impre=1
                zi(inec-1+iec) =dg(iec)
            endif
 15     continue
!
! --  TRI DES COMPOSANTES DANS L'INTERVALLE BORINF,BORSUP
!
        if (lsup .or. linf) then
            do 35 iva = 1, icompt
                if (lsup) then
                    if ((zr(irval-1+iva)-borsup) .gt. 0.d0) zi(ipos-1+ iva)=0
                endif
                if (linf) then
                    if ((zr(irval-1+iva)-borinf) .lt. 0.d0) zi(ipos-1+ iva)=0
                endif
 35         continue
!
! --- RETASSAGE POUR IMPRIMER COMPOSANTES PRESENTES DANS L'INTERVALLE --
!
            icomp2=0
            do 36 i = 1, icompt
                if (zi(ipos-1+i) .ne. 0) then
                    icomp2=icomp2+1
                    zi(ipos-1+icomp2)=zi(ipos-1+i)
                    zr(irval-1+icomp2)=zr(irval-1+i)
                endif
 36         continue
            icompt=icomp2
        endif
        if (icompt .eq. 0) then
            goto 11
        endif
!
! -- RECHERCHE DE LA VALEURE MAXIMALE ---
!
        if (lmax) then
            do 90 i = 1, icompt
                if (zr(imax-1+zi(ipos-1+i)) .eq. rundf) then
                    zr(imax-1+zi(ipos-1+i)) = zr(irval-1+i)
                    zk8(inmax-1+zi(ipos-1+i)) = nomnoe(inno)
                    zi(ivmax-1+zi(ipos-1+i)) = 1
                else if (zr(irval-1+i).gt.zr(imax-1+zi(ipos-1+i))) then
                    zr(imax-1+zi(ipos-1+i))= zr(irval-1+i)
                    zk8(inmax-1+zi(ipos-1+i)) = nomnoe(inno)
                    zi(ivmax-1+zi(ipos-1+i)) = 1
                else if (zr(irval-1+i).eq.zr(imax-1+zi(ipos-1+i))) then
                    zi(ivmax-1+zi(ipos-1+i))=zi(ivmax-1+zi(ipos-1+i))+&
                    1
                endif
 90         continue
        endif
!
! -- RECHERCHE DE LA VALEURE MINIMALE ---
!
        if (lmin) then
            do 91 i = 1, icompt
                if (zr(imin-1+zi(ipos-1+i)) .eq. rundf) then
                    zr(imin-1+zi(ipos-1+i)) = zr(irval-1+i)
                    zk8(inmin-1+zi(ipos-1+i)) = nomnoe(inno)
                    zi(ivmin-1+zi(ipos-1+i)) = 1
                else if (zr(irval-1+i).lt.zr(imin-1+zi(ipos-1+i))) then
                    zr(imin-1+zi(ipos-1+i))= zr(irval-1+i)
                    zk8(inmin-1+zi(ipos-1+i)) = nomnoe(inno)
                    zi(ivmin-1+zi(ipos-1+i)) = 1
                else if (zr(irval-1+i).eq.zr(imin-1+zi(ipos-1+i))) then
                    zi(ivmin-1+zi(ipos-1+i))=zi(ivmin-1+zi(ipos-1+i))+&
                    1
                endif
 91         continue
        endif
!
! - IMPRESSION DES VALEURS ---
!
        if (.not.lmax .and. .not.lmin .and. lcor) then
            ilign=(icompt+ndim)/6
            irest=(icompt+ndim)-ilign*6
            if (impre .eq. 1 .or. lsup .or. linf) then
                fmt = ' '
                if (irest .ne. 0) then
                    fmt = '( 1X,A,6(1X,'//forcmp//'),30(/,9X,6(1X,'// forcmp//')) )'
                else if (irest.eq.0.and.ilign.eq.1) then
                    fmt = '(1X,A,6(1X,'//forcmp//'))'
                else
                    write(fmt,'(A,A8,A,I2,A,A8,A)') '(1X,A,6(1X,', forcmp,&
     &                     '),', (ilign-1), '(/,9X,6(1X,', forcmp, ')))'
                endif
                write (ifi,fmt) 'NOEUD   ', (nomcor(i),i=1,ndim),&
     &                        (nomcmp(zi(ipos-1+i)),i=1,icompt)
            endif
            fmt = ' '
            if (irest .ne. 0) then
                fmt = '(1X,A,6(1X,'//format// '),30(/,9X,6(1X,'// format//')))'
            else if (irest.eq.0.and.ilign.eq.1) then
                fmt = '(1X,A,6(1X,'//format//'))'
            else
                write(fmt,'(A,A10,A,I2,A,A10,A)') '(1X,A,6(1X,',&
                format, '),',(ilign-1),'(/,9X,6(1X,', format, ')))'
            endif
            write (ifi,fmt) nomnoe(inno), (coor((ino-1)*3+i),i=1,ndim)&
            ,(zr(irval-1+i),i=1,icompt)
        else if (.not.lmax.and..not.lmin) then
            ilign=(icompt)/6
            irest=(icompt)-ilign*6
            if (impre .eq. 1 .or. lsup .or. linf) then
                fmt = ' '
                if (irest .ne. 0) then
                    fmt = '( 1X,A,6(1X,'//forcmp//'),30(/,9X,6(1X,'// forcmp//')) )'
                else if (irest.eq.0.and.ilign.eq.1) then
                    fmt = '(1X,A,6(1X,'//forcmp//'))'
                else
                    write(fmt,'(A,A8,A,I2,A,A8,A)') '(1X,A,6(1X,',forcmp,&
     &                    '),', (ilign-1), '(/,9X,6(1X,', forcmp, ')))'
                endif
                write (ifi,fmt) 'NOEUD   ',&
     &                        (nomcmp(zi(ipos-1+i)),i=1,icompt)
            endif
            fmt = ' '
            if (irest .ne. 0) then
                fmt = '(1X,A,6(1X,'//format// '),30(/,9X,6(1X,'// format//')))'
            else if (irest.eq.0.and.ilign.eq.1) then
                fmt = '(1X,A,6(1X,'//format//'))'
            else
                write(fmt,'(A,A10,A,I2,A,A10,A)') '(1X,A,6(1X,',&
                format, '),',(ilign-1),'(/,9X,6(1X,', format, ')))'
            endif
            write (ifi,fmt) nomnoe(inno), (zr(irval-1+i),i=1,icompt)
        endif
 11 end do
    write (ifi,'(A)') ' '
!
! --- IMPRESSION DE LA VALEUR MAXIMALE ---
!
    if (lmax) then
        do 95 i = 1, ncmpmx
            if (zr(imax-1+i) .ne. rundf) then
                form1 = '(1X,3A,1X,'//format//',A,I4,A,A8)'
                write(ifi,form1) 'LA VALEUR MAXIMALE DE ', nomcmp(i),&
     &       ' EST',zr(imax-1+i),&
     &       ' EN ',zi(ivmax-1+i),' NOEUD(S) : ',zk8(inmax-1+i)
            endif
 95     continue
    endif
!
! --- IMPRESSION DE LA VALEUR MINIMALE ---
!
    if (lmin) then
        do 96 i = 1, ncmpmx
            if (zr(imin-1+i) .ne. rundf) then
                form1 = '(1X,3A,1X,'//format//',A,I4,A,A8)'
                write(ifi,form1) 'LA VALEUR MINIMALE DE ',nomcmp(i),&
     &       ' EST',zr(imin-1+i),&
     &       ' EN ',zi(ivmin-1+i),' NOEUD(S) : ',zk8(inmin-1+i)
            endif
 96     continue
    endif
!
    call jedetr('&&IRCNRL.VAL')
    call jedetr('&&IRCNRL.POS')
    call jedetr('&&IRCNRL.ENT')
    call jedetr('&&IRCNRL.MAX')
    call jedetr('&&IRCNRL.NOEMAX')
    call jedetr('&&IRCNRL.NBVMAX')
    call jedetr('&&IRCNRL.MIN')
    call jedetr('&&IRCNRL.NOEMIN')
    call jedetr('&&IRCNRL.NBVMIN')
    call jedema()
end subroutine

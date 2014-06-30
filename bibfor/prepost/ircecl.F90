subroutine ircecl(ifi, nbel, ligrel, nbgrel, longr,&
                  ncmpmx, vale, nomcmp, nomel, loc,&
                  celd, connex, point, nomnos, nbcmpt,&
                  nucmpu, nbnot, numnoe, nbmat, nummai,&
                  lsup, borsup, linf, borinf, lmax,&
                  lmin, lcor, ndim, coor, nolili,&
                  formr, ncmpv, nucmp)
! aslint: disable=W1501,W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/codent.h"
#include "asterfort/dgmode.h"
#include "asterfort/digdel.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: ifi, nbel, ligrel(*), nbgrel, longr(*), ncmpmx, nbnot, nbcmpt
    integer :: nucmpu(*), celd(*), connex(*), point(*), numnoe(*), nbmat, ndim
    integer :: nummai(*), ncmpv, nucmp(*)
    real(kind=8) :: borsup, borinf, coor(*)
    complex(kind=8) :: vale(*)
    character(len=*) :: nomcmp(*), nomel(*), loc, nomnos(*), formr
    character(len=19) :: nolili
    logical(kind=1) :: lsup, linf, lmax, lmin, lcor
!     ------------------------------------------------------------------
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
!        ECRITURE D'UN CHAMELEM SUR LISTING
!        A VALEURS COMPLEXES
!  ENTREE:
!     IFI   : UNITE LOGIQUE DU FICHIER
!     NBEL  : NOMBRE D'ELEMENTS DU LIGREL ( DU MAILLAGE)
!     LIGREL: LIGREL COMPLET
!     NBGREL: NOMBRE DE GRELS
!     LONGR : POINTEUR DE LONGUEUR DE LIGREL
!     NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
!     VALE  : VALEURS DU CHAM_NO
!     NOMCMP: NOMS DES CMP
!     NOMEL : NOMS DES MAILLES SUPPORTS DES ELEMENTS
!     LOC   : LOCALISATION DES VALEURS (ELNO OU ELGA OU ELEM)
!     CELD  : DESCRIPTEUR DU CHAM_ELEM (MODES LOCAUX,ADRESSES->.CELV)
!     CONNEX: CONNECTIVITES DES MAILLES
!     POINT : POINTEUR DANS LES CONNECTIVITES
!     NOMNOS: NOMS DES NOEUDS
!     NBCMPT: NOMBRE DE COMPOSANTES A IMPRIMER
!     NUCMPU: NUMEROS DES COMPOSANTES A IMPRIMER
!     NBMAT : NOMBRE DE MAILLES OU ON DESIRE IMPRIMER LE CHAMELEM
!     NUMMAI: NUMEROS DES MAILLES OU ON DESIRE IMPRIMER LE CHAMELEM
!     LSUP  : =.TRUE.  INDIQUE PRESENCE D'UNE BORNE SUPERIEURE
!     BORSUP: VALEUR DE LA BORNE SUPERIEURE
!     LINF  : =.TRUE.  INDIQUE PRESENCE D'UNE BORNE INFERIEURE
!     BORINF: VALEUR DE LA BORNE INFERIEURE
!     LMAX  : =.TRUE.  INDIQUE IMPRESSION VALEUR MAXIMALE
!     LMIN  : =.TRUE.  INDIQUE IMPRESSION VALEUR MINIMALE
!     LCOR  : =.TRUE.  IMPRESSION DES COORDONNEES DE NOEUDS DEMANDEE
!     NDIM  : DIMENSION DU PROBLEME
!     COOR  : TABLEAU DES COORDONNEES DE NOEUDS
!     NOLILI: NOM DU LIGREL
!     FORMR : FORMAT D'ECRITURE DES REELS SUR "RESULTAT"
!     ------------------------------------------------------------------
    integer :: ilong, imodel
    real(kind=8) :: rundf, value, valmax, valmin
    character(len=3) :: cbid
    character(len=8) :: nomno, nomcp, kbid, forcmp, nomcor(3)
    character(len=10) :: format
    character(len=24) :: nrepe
    character(len=50) :: fmt, fmv, fmt1, fmt2, fmt3, fmv2, form1
    logical(kind=1) :: limpr
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, i2, iachml, iad, iadr, iaec, icm
    integer :: icmax, icmin, icmp, icmp2, icoe, icoef, icoef2
    integer :: icomax, icomp2, icou, icval, id, iel, ielg
    integer :: if, igre, igrel, iino, ilig, imai, imail
    integer :: in, inmax, inmin, inom, inop, inot, inu
    integer :: ipca, ipo2, ipoin, ipoin1, iposg, iposv, irepe
    integer :: ires, irmax, irmin, irval, iva, ivmax, ivmin
    integer :: j, jco, jmod, lgr, mode, modsau
    integer :: nbcpt, nbno, ncmp, ncmp2, ncmpp, ncou, nec
    integer :: npcalc, nsca, nscal, nuno
!-----------------------------------------------------------------------
    call jemarq()
    kbid='        '
    rundf = r8vide()
    nomcor(1) = 'X'
    nomcor(2) = 'Y'
    nomcor(3) = 'Z'
    format = formr
    lgr = lxlgut( format )
    id = 0
    if = 0
    call jeveuo('&CATA.TE.MODELOC', 'L', imodel)
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', ilong)
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
 2  end do
    if (id .ne. 0 .and. if .ge. id) then
        forcmp = 'A'//format(id:if)
    else
        forcmp = 'A12'
    endif
!
!  -- DETERMINATION DU NOMBRE MAXIMUM DE SOUS_POINTS ---
    icomax = 0
    do 4 igre = 1, nbgrel
        icoef=max(1,celd(4))
        if (icoef .gt. icomax) icomax = icoef
 4  end do
    ncmp = ncmpv
    if (ncmp .gt. 0) then
        ncmp = 0
        do 141 i = 1, ncmpv
            if (nucmp(i) .le. icomax) then
                ncmp = ncmp + 1
            else
                call codent(nucmp(i), 'G', cbid)
                nomcp = 'V'//cbid
                call utmess('A', 'PREPOST_74', sk=nomcp)
            endif
141      continue
        if (ncmp .eq. 0) then
            call utmess('A', 'PREPOST_75')
            goto 9999
        endif
        icomax = ncmp
    endif
!
    if (lmax .or. lmin) then
        call jedetr('&&IRCECL.NCMT')
        call wkvect('&&IRCECL.NCMT', 'V V K16', ncmpmx*icomax, inot)
        do 6 i = 1, ncmpmx
            if (icomax .gt. 1 .or. ncmp .ge. 1) then
                do 7 jco = 1, icomax
                    if (ncmp .gt. 0) then
                        call codent(nucmp(jco), 'G', cbid)
                    else
                        call codent(jco, 'G', cbid)
                    endif
                    nomcp = nomcmp(i)
                    zk16(inot-1+(i-1)*icomax+jco) = 'V'//cbid
 7              continue
            else
                zk16(inot-1+i)=nomcmp(i)
            endif
 6      continue
    endif
    if (lmax) then
        call jedetr('&&IRCECL.MAXR')
        call wkvect('&&IRCECL.MAXR', 'V V R', ncmpmx*icomax, irmax)
        call jedetr('&&IRCECL.MAXC')
        call wkvect('&&IRCECL.MAXC', 'V V R', ncmpmx*icomax, icmax)
        call jedetr('&&IRCECL.MAIMAX')
        call wkvect('&&IRCECL.MAIMAX', 'V V K8', ncmpmx*icomax, inmax)
        call jedetr('&&IRCECL.NBVMAX')
        call wkvect('&&IRCECL.NBVMAX', 'V V I', ncmpmx*icomax, ivmax)
        do 90 i = 1, ncmpmx*icomax
            zr(irmax-1+i)=rundf
90      continue
    endif
    if (lmin) then
        call jedetr('&&IRCECL.MINR')
        call wkvect('&&IRCECL.MINR', 'V V R', ncmpmx*icomax, irmin)
        call jedetr('&&IRCECL.MINC')
        call wkvect('&&IRCECL.MINC', 'V V R', ncmpmx*icomax, icmin)
        call jedetr('&&IRCECL.MAIMIN')
        call wkvect('&&IRCECL.MAIMIN', 'V V K8', ncmpmx*icomax, inmin)
        call jedetr('&&IRCECL.NBVMIN')
        call wkvect('&&IRCECL.NBVMIN', 'V V I', ncmpmx*icomax, ivmin)
        do 91 i = 1, ncmpmx*icomax
            zr(irmin-1+i)=rundf
91      continue
    endif
    if (loc .eq. 'ELGA' .or. loc .eq. 'ELEM' .or. .not.lcor) ndim = 0
    nrepe = nolili//'.REPE'
    call jeveuo(nrepe, 'L', irepe)
    if (nbmat .ne. 0) nbel=nbmat
    modsau = 0
    do 12 imai = 1, nbel
        if (nbmat .ne. 0) then
            imail = nummai(imai)
        else
            imail = imai
        endif
        igrel = zi(irepe+2*(imail-1)+1-1)
        if (igrel .eq. 0) goto 12
        ielg = zi(irepe+2*(imail-1)+2-1)
        mode=celd(celd(4+igrel)+2)
        if (mode .eq. 0) goto 12
        if (mode .ne. modsau) then
            ipoin1=longr(igrel)
            call jeveuo(jexnum('&CATA.TE.MODELOC', mode), 'L', jmod)
            nec = nbec(zi(jmod-1+2))
            call jedetr('&&IRCECL.ENT_COD')
            call wkvect('&&IRCECL.ENT_COD', 'V V I', nec, iaec)
            call dgmode(mode, imodel, ilong, nec, zi(iaec))
            iad=celd(celd(4+igrel)+8)
            nscal = digdel(mode)
            icoef=max(1,celd(4))
            nsca = nscal*icoef
            icoef2 = icoef
            if (ncmp .gt. 0) icoef2 = ncmp
            ncmpp = 0
            ncmp2 = 0
!
! -- IPOSG : POSITION DE LA COMPOSANTE DANS LA GRANDEUR
! -- IPOSV : POSITION DE LA COMPOSANTE DANS LE .VALE
!
            call jedetr('&&IRCECL.POSG')
            call wkvect('&&IRCECL.POSG', 'V V I', ncmpmx*icoef2, iposg)
            call jedetr('&&IRCECL.POSV')
            call wkvect('&&IRCECL.POSV', 'V V I', ncmpmx, iposv)
            call jedetr('&&IRCECL.COEF')
            call wkvect('&&IRCECL.COEF', 'V V I', ncmpmx*icoef2, icoe)
            call jedetr('&&IRCECL.NCMP')
            call wkvect('&&IRCECL.NCMP', 'V V K16', ncmpmx*icoef2, inom)
            if (lsup .or. linf) then
                call jedetr('&&IRCECL.NCPP')
                call wkvect('&&IRCECL.NCPP', 'V V K16', ncmpmx*icoef2, inop)
                call jedetr('&&IRCECL.PO2')
                call wkvect('&&IRCECL.PO2', 'V V I', ncmpmx*icoef2, ipo2)
            endif
            call jedetr('&&IRCECL.VALR')
            call wkvect('&&IRCECL.VALR', 'V V R', ncmpmx*icoef2, irval)
            call jedetr('&&IRCECL.VALC')
            call wkvect('&&IRCECL.VALC', 'V V R', ncmpmx*icoef2, icval)
            do 5 i = 1, ncmpmx*icoef2
                zi(iposg-1+i)=0
 5          continue
            do 26 i = 1, ncmpmx
                zi(iposv-1+i)=0
26          continue
            do 23 i = 1, ncmpmx
                if (exisdg(zi(iaec),i)) then
                    ncmpp=ncmpp+1
                    if (nbcmpt .ne. 0) then
                        do 8 icm = 1, nbcmpt
                            icmp2=nucmpu(icm)
                            if (i .eq. icmp2) then
                                ncmp2=ncmp2+1
                                do 92 jco = 1, icoef2
                                    zi(iposg-1+(icm-1)*icoef2+jco)=i
92                              continue
                                zi(iposv-1+icm) = ncmpp
                            endif
 8                      continue
                    else
                        do 93 jco = 1, icoef2
                            zi(iposg-1+(ncmpp-1)*icoef2+jco)=i
93                      continue
                    endif
                endif
23          continue
            if (nbcmpt .eq. 0) ncmp2=ncmpp
            npcalc = nscal/ncmpp
!
! --- RETASSAGE DU TABLEAU DES POSITIONS DES COMPOSANTES DANS GRANDEUR-
!
            if (nbcmpt .ne. 0) then
                i2=0
                do 9 i = 1, nbcmpt*icoef2
                    if (zi(iposg-1+i) .ne. 0) then
                        i2=i2+1
                        zi(iposg-1+i2)= zi(iposg-1+i)
                    endif
 9              continue
            endif
!
! --- STOCKAGE DES NOMS DE COMPOSANTES ---
            do 42 i = 1, ncmp2
                if (icoef2 .gt. 1 .or. ncmp .ge. 1) then
                    do 43 jco = 1, icoef2
                        if (ncmp .gt. 0) then
                            call codent(nucmp(jco), 'G', cbid)
                        else
                            call codent(jco, 'G', cbid)
                        endif
                        nomcp = nomcmp(zi(iposg-1+i))
                        zk16(inom-1+(i-1)*icoef2+jco) = 'V'//cbid
43                  continue
                else
                    zk16(inom-1+i)=nomcmp(zi(iposg-1+i))
                endif
42          continue
!
! --- CREATION DES FORMATS D'ECRITURE ---
!
            if (.not.lmax .and. .not.lmin) then
                ilig=(ncmp2*icoef2+ndim)/6
                ires=(ncmp2*icoef2+ndim)-ilig*6
                fmt = ' '
                fmv = ' '
                fmv2 = ' '
                if (ires .ne. 0) then
                    fmt = '( 1X, A8, 6(1X, '//forcmp//'), 30(/, 9X, 6(1X, '// forcmp//')) )'
                    if (loc .eq. 'ELNO') then
                        fmv = '( 1X, A8, 6(1X, '//format//'), 30(/, 9X, 6(1X, '// format//')) )'
                    else if (loc.eq.'ELGA') then
                        fmv = '( 2X, I7, 6(1X, '//format//'), 30(/, 9X, 6(1X, '// format//')) )'
                        fmv2 = '( 9X, 6(1X, '//format//'), 30(/, 9X, 6(1X, '// format//')) )'
                    else if (loc.eq.'ELEM') then
                        fmv = '( 9X, 6(1X, '//format//'), 30(/, 9X, 6(1X, '// format//')) )'
                        fmv2 = '( 9X, 6(1X, '//format//'), 30(/, 9X, 6(1X, '// format//')) )'
                    endif
                else if (ires.eq.0.and.ilig.eq.1) then
                    fmt = '(1X,A8,6(1X,'//forcmp//'))'
                    if (loc .eq. 'ELNO') then
                        fmv = '(1X,A8,6(1X,'//format//'))'
                    else if (loc.eq.'ELGA') then
                        fmv = '(2X,I7,6(1X,'//format//'))'
                        fmv2 = '(9X,6(1X,'//format//'))'
                    else if (loc.eq.'ELEM') then
                        fmv = '(9X,6(1X,'//format//'))'
                        fmv2 = '(9X,6(1X,'//format//'))'
                    endif
                else
                    write(fmt,'(A,A8,A,I2,A,A8,A)') '(1X,A8,6(1X,',&
                    forcmp, '),',(ilig-1),'(/,9X,6(1X,',forcmp,')))'
                    if (loc .eq. 'ELNO') then
                        write(fmv,'(A,A10,A,I2,A,A10,A)') '(1X,A8,6(1X,',format,&
     &                   '),',(ilig-1),'(/,9X,6(1X,',format,')))'
                    else if (loc.eq.'ELGA') then
                        write(fmv,'(A,A10,A,I2,A,A10,A)') '(2X,I7,6(1X,',format,&
     &                   '),',(ilig-1),'(/,9X,6(1X,',format,')))'
                        write(fmv2,'(A,A10,A,I2,A,A10,A)') '(9X,6(1X,',format,&
     &                    '),',(ilig-1),'(/,9X,6(1X,',format,')))'
                    else if (loc.eq.'ELEM') then
                        write(fmv ,'(A,A10,A,I2,A,A10,A)') '(9X,6(1X,',format,&
     &                    '),',(ilig-1),'(/,9X,6(1X,',format,')))'
                        write(fmv2,'(A,A10,A,I2,A,A10,A)') '(9X,6(1X,',format,&
     &                    '),',(ilig-1),'(/,9X,6(1X,',format,')))'
                    endif
                endif
            endif
        endif
!
! --- BOUCLE SUR LES ELEMENTS ---
!
        iel=ligrel(ipoin1+ielg-1)
        limpr = .true.
        if (.not.lsup .and. .not.linf .and. .not.lmax .and. .not.lmin) then
            if (ndim .eq. 0) then
                write(ifi,fmt) nomel(iel),(zk16(inom-1+i)(1:11),&
                i=1,icoef2*ncmp2)
            else
                write(ifi,fmt) nomel(iel),(nomcor(i),i=1,ndim),&
                (zk16(inom-1+i)(1:11),i=1,icoef2*ncmp2)
            endif
        endif
        iachml = iad + nsca * (ielg-1)
        if (loc .eq. 'ELGA' .or. loc .eq. 'ELEM') then
            do 16 ipca = 1, npcalc
                j=iachml-1+ncmpp*icoef*(ipca-1)
                if (nbcmpt .eq. 0) then
                    do 10 i = 1, ncmp2
                        if (ncmp .gt. 0) then
                            do 551 jco = 1, icoef2
                                zr(irval-1+(i-1)*icoef2+jco)= dble(&
                                vale(j+i+(nucmp(jco)-1)*ncmpp))
                                zr(icval-1+(i-1)*icoef2+jco)= dimag(&
                                vale(j+i+(nucmp(jco)-1)*ncmpp))
                                zi(icoe-1+(i-1)*icoef2+jco)=jco
551                          continue
                        else
                            do 55 jco = 1, icoef2
                                zr(irval-1+(i-1)*icoef2+jco)= dble(&
                                vale(j+i+(jco-1)*ncmpp))
                                zr(icval-1+(i-1)*icoef2+jco)= dimag(&
                                vale(j+i+(jco-1)*ncmpp))
                                zi(icoe-1+(i-1)*icoef2+jco)=jco
55                          continue
                        endif
10                  continue
                else
                    do 20 i = 1, ncmp2
                        inu=zi(iposv-1+i)
                        if (ncmp .gt. 0) then
                            do 301 jco = 1, icoef2
                                zr(irval-1+(i-1)*icoef2+jco)= dble(&
                                vale(j+inu+(nucmp(jco)-1)*ncmpp))
                                zr(icval-1+(i-1)*icoef2+jco)= dimag(&
                                vale(j+inu+(nucmp(jco)-1)*ncmpp))
                                zi(icoe-1+(i-1)*icoef2+jco)=jco
301                          continue
                        else
                            do 30 jco = 1, icoef2
                                zr(irval-1+(i-1)*icoef2+jco)= dble(&
                                vale(j+inu+(jco-1)*ncmpp))
                                zr(icval-1+(i-1)*icoef2+jco)= dimag(&
                                vale(j+inu+(jco-1)*ncmpp))
                                zi(icoe-1+(i-1)*icoef2+jco)=jco
30                          continue
                        endif
20                  continue
                endif
!
! --  TRI DES COMPOSANTES DANS L'INTERVALLE BORINF,BORSUP
!
                if (lsup .or. linf) then
                    do 35 iva = 1, icoef2*ncmp2
                        value = sqrt(zr(irval-1+iva)**2+zr(icval-1+ iva)**2)
                        if (lsup) then
                            if ((value-borsup) .gt. 0.d0) zi(icoe-1+iva)= 0
                        endif
                        if (linf) then
                            if ((value-borinf) .lt. 0.d0) zi(icoe-1+iva)= 0
                        endif
35                  continue
!
! --- RETASSAGE POUR IMPRIMER COMPOSANTES PRESENTES DANS L'INTERVALLE --
!
                    icomp2=0
                    do 36 i = 1, icoef2*ncmp2
                        if (zi(icoe-1+i) .ne. 0) then
                            icomp2=icomp2+1
                            zi(icoe-1+icomp2)=zi(icoe-1+i)
                            zi(ipo2-1+icomp2)=zi(iposg-1+i)
                            zr(irval-1+icomp2)=zr(irval-1+i)
                            zr(icval-1+icomp2)=zr(icval-1+i)
                            zk16(inop-1+icomp2)=zk16(inom-1+i)
                        endif
36                  continue
                    if (icomp2 .eq. 0) goto 16
!
! -- IMPRESSION ----
!
                    if (.not.lmax .and. .not.lmin) then
                        ilig=(icomp2)/6
                        ires=(icomp2)-ilig*6
                        fmt1 = ' '
                        fmt2 = ' '
                        fmt3 = ' '
                        if (loc .eq. 'ELGA') then
                            if (ires .ne. 0) then
                                fmt1 = '(9X, 6(1X, '//forcmp//'), 30(/, 9X, 6(1X, '//forcmp//')))'
                                fmt2 = '(2X, I7, 6(1X, '//format//'),30(/, 9X, 6(1X, '&
                                       //format//')))'
                                fmt3 = '(9X, 6(1X, '//format//'), 30(/, 9X, 6(1X, ' //format//')))'
                            else if (ires.eq.0.and.ilig.eq.1) then
                                fmt1 = '(9X,6(1X,'//forcmp//'))'
                                fmt2 = '(2X,I7,6(1X,'//format//'))'
                                fmt3 = '(9X,6(1X,'//format//'))'
                            else
                                write(fmt1,'(A,A8,A,I2,A,A8,A)') '(1X,A8,6(1X,',&
     &                   forcmp,'),',(ilig-1),'(/,9X,6(1X,',forcmp,')))'
                                write(fmt2,'(A,A10,A,I2,A,A10,A)')'(2X,I7,6(1X,',&
     &                   format,'),',(ilig-1),'(/,9X,6(1X,',format,')))'
                                write(fmt3,'(A,A10,A,I2,A,A10,A)') '(9X,6(1X,',&
     &                   format,'),',(ilig-1),'(/,9X,6(1X,',format,')))'
                            endif
                        else
                            if (ires .ne. 0) then
                                fmt1 = '(9X, 6(1X, '//forcmp//'), 30(/, 9X, 6(1X, ' //forcmp//')))'
                                fmt2 = '(9X, 6(1X, '//format//'), 30(/, 9X, 6(1X, ' //format//')))'
                                fmt3 = '(9X, 6(1X, '//format//'), 30(/, 9X, 6(1X, ' //format//')))'
                            else if (ires.eq.0.and.ilig.eq.1) then
                                fmt1 = '(9X,6(1X,'//forcmp//'))'
                                fmt2 = '(9X,6(1X,'//format//'))'
                                fmt3 = '(9X,6(1X,'//format//'))'
                            else
                                write(fmt1,'(A,A8,A,I2,A,A8,A)') '(1X,A8,6(1X,',&
     &                   forcmp,'),',(ilig-1),'(/,9X,6(1X,',forcmp,')))'
                                write(fmt2,'(A,A10,A,I2,A,A10,A)')'(9X,6(1X,',&
     &                   format,'),',(ilig-1),'(/,9X,6(1X,',format,')))'
                                write(fmt3,'(A,A10,A,I2,A,A10,A)') '(9X,6(1X,',&
     &                   format,'),',(ilig-1),'(/,9X,6(1X,',format,')))'
                            endif
                        endif
                        if (lsup .or. linf) then
                            if (limpr) then
                                write(ifi,'(A,I2,A)') nomel(iel)
                                limpr=.false.
                            endif
                        endif
                        write(ifi,fmt1) (zk16(inop-1+i)(1:11),i=1,&
                        icomp2)
                        write(ifi,fmt2) ipca,(zr(irval-1+icmp),&
                        icmp=1,icomp2)
                        write(ifi,fmt3) (zr(icval-1+icmp), icmp=1,&
                        icomp2)
                    endif
                    nbcpt=icomp2
                else
                    if (.not.lmax .and. .not.lmin) then
                        if (loc .eq. 'ELGA') then
                            write(ifi,fmv) ipca,(zr(irval-1+icmp),&
                            icmp=1,icoef2*ncmp2)
                            write(ifi,fmv2) (zr(icval-1+icmp),&
                            icmp=1,icoef2*ncmp2)
                        else
                            write(ifi,fmv) (zr(irval-1+icmp), icmp=1,&
                            icoef2*ncmp2)
                            write(ifi,fmv2) (zr(icval-1+icmp),&
                            icmp=1,icoef2*ncmp2)
                        endif
                    endif
                    nbcpt=icoef2*ncmp2
                endif
!
! -- RECHERCHE DE LA VALEUR MAXIMALE ---
!
                if (lmax) then
                    do 101 i = 1, nbcpt
                        if (lsup .or. linf) then
                            iadr=(zi(ipo2-1+i)-1)*icoef2+zi(icoe-1+i)
                        else
                            iadr=(zi(iposg-1+i)-1)*icoef2+zi(icoe-1+i)
                        endif
                        if (zr(irmax-1+iadr) .eq. rundf) then
                            zr(irmax-1+iadr) = zr(irval-1+i)
                            zr(icmax-1+iadr) = zr(icval-1+i)
                            zk8(inmax-1+iadr) = nomel(iel)
                            zi(ivmax-1+iadr) = 1
                        else
                            valmax = sqrt( zr(irmax-1+iadr)**2 + zr(icmax-1+iadr)**2)
                            value = sqrt(zr(irval-1+i)**2 + zr(icval- 1+i)**2)
                            if (value .gt. valmax) then
                                zr(irmax-1+iadr)= zr(irval-1+i)
                                zr(icmax-1+iadr)= zr(icval-1+i)
                                zk8(inmax-1+iadr) = nomel(iel)
                                zi(ivmax-1+iadr) = 1
                            else if (value.eq.valmax) then
                                zi(ivmax-1+iadr)=zi(ivmax-1+iadr)+1
                            endif
                        endif
101                  continue
                endif
!
! -- RECHERCHE DE LA VALEURE MINIMALE ---
!
                if (lmin) then
                    do 102 i = 1, nbcpt
                        if (lsup .or. linf) then
                            iadr=(zi(ipo2-1+i)-1)*icoef2+zi(icoe-1+i)
                        else
                            iadr=(zi(iposg-1+i)-1)*icoef2+zi(icoe-1+i)
                        endif
                        if (zr(irmin-1+iadr) .eq. rundf) then
                            zr(irmin-1+iadr) = zr(irval-1+i)
                            zr(icmin-1+iadr) = zr(icval-1+i)
                            zk8(inmin-1+iadr) = nomel(iel)
                            zi(ivmin-1+iadr) = 1
                        else
                            valmin = sqrt( zr(irmin-1+iadr)**2 + zr(icmin-1+iadr)**2)
                            value = sqrt(zr(irval-1+i)**2 + zr(icval- 1+i)**2)
                            if (value .lt. valmin) then
                                zr(irmin-1+iadr)= zr(irval-1+i)
                                zr(icmin-1+iadr)= zr(icval-1+i)
                                zk8(inmin-1+iadr) = nomel(iel)
                                zi(ivmin-1+iadr) = 1
                            else if (value.eq.valmin) then
                                zi(ivmin-1+iadr)=zi(ivmin-1+iadr)+1
                            endif
                        endif
102                  continue
                endif
16          continue
!CCCCC
        else if (loc.eq.'ELNO') then
            ipoin=point(iel)
            nbno=point(iel+1)-ipoin
            ncou=npcalc/nbno
            do 17 icou = 1, ncou
                if (ncou .gt. 1) then
                    if (.not.lmax .and. .not.lmin) then
                        if (ncou .eq. 2) then
                            if (icou .eq. 1) write(ifi,'(A)') ' PEAU INTERNE'
                            if (icou .eq. 2) write(ifi,'(A)') ' PEAU EXTERNE'
                        else
                            write(ifi,'(A,I3)') ' COUCHE NUMERO:',&
                            icou
                        endif
                    endif
                endif
                do 18 in = 1, nbno
                    nuno = connex(ipoin-1+in)
                    if (nbnot .ne. 0) then
                        do 187 iino = 1, nbnot
                            if (nuno .eq. numnoe(iino)) goto 189
187                      continue
                        goto 18
189                      continue
                    endif
                    nomno= nomnos(nuno)
                    j=iachml-1+ncmpp*icoef*(in-1) +(icou-1)*ncmpp*&
                    icoef*nbno
                    if (nbcmpt .eq. 0) then
                        do 50 i = 1, ncmp2
                            if (ncmp .gt. 0) then
                                do 511 jco = 1, icoef2
                                    zr(irval-1+(i-1)*icoef2+jco)=&
                                    dble(vale(j+i+(nucmp(jco)-1)*&
                                    ncmpp))
                                    zr(icval-1+(i-1)*icoef2+jco)=&
                                    dimag(vale(j+i+(nucmp(jco)-1)*&
                                    ncmpp))
                                    zi(icoe-1+(i-1)*icoef2+jco)=jco
511                              continue
                            else
                                do 51 jco = 1, icoef2
                                    zr(irval-1+(i-1)*icoef2+jco)=&
                                    dble(vale(j+i+(jco-1)*ncmpp))
                                    zr(icval-1+(i-1)*icoef2+jco)=&
                                    dimag(vale(j+i+(jco-1)*ncmpp))
                                    zi(icoe-1+(i-1)*icoef2+jco)=jco
51                              continue
                            endif
50                      continue
                    else
                        do 60 i = 1, ncmp2
                            inu=zi(iposv-1+i)
                            if (ncmp .gt. 0) then
                                do 701 jco = 1, icoef2
                                    zr(irval-1+(i-1)*icoef2+jco)=&
                                    dble(vale(j+inu+(nucmp(jco)-1)*&
                                    ncmpp))
                                    zr(icval-1+(i-1)*icoef2+jco)=&
                                    dimag(vale(j+inu+(nucmp(jco)-1)*&
                                    ncmpp))
                                    zi(icoe-1+(i-1)*icoef2+jco)=jco
701                              continue
                            else
                                do 70 jco = 1, icoef2
                                    zr(irval-1+(i-1)*icoef2+jco)=&
                                    dble(vale(j+inu+(jco-1)*ncmpp))
                                    zr(icval-1+(i-1)*icoef2+jco)=&
                                    dimag(vale(j+inu+(jco-1)*ncmpp))
                                    zi(icoe-1+(i-1)*icoef2+jco)=jco
70                              continue
                            endif
60                      continue
                    endif
!
! --  TRI DES COMPOSANTES DANS L'INTERVALLE BORINF,BORSUP
!
                    if (lsup .or. linf) then
                        do 65 iva = 1, icoef2*ncmp2
                            value= sqrt(zr(irval-1+iva)**2+ zr(icval-&
                            1+iva)**2)
                            if (lsup) then
                                if ((value-borsup) .gt. 0.d0) zi(icoe-1+ iva)=0
                            endif
                            if (linf) then
                                if ((value-borinf) .lt. 0.d0) zi(icoe-1+ iva)=0
                            endif
65                      continue
!
! --- RETASSAGE POUR IMPRIMER COMPOSANTES PRESENTES DANS L'INTERVALLE --
!
                        icomp2=0
                        do 66 i = 1, icoef2*ncmp2
                            if (zi(icoe-1+i) .ne. 0) then
                                icomp2=icomp2+1
                                zi(icoe-1+icomp2)=zi(icoe-1+i)
                                zi(ipo2-1+icomp2)=zi(iposg-1+i)
                                zr(irval-1+icomp2)=zr(irval-1+i)
                                zr(icval-1+icomp2)=zr(icval-1+i)
                                zk16(inop-1+icomp2)=zk16(inom-1+i)
                            endif
66                      continue
                        if (icomp2 .eq. 0) goto 18
!
! -- IMPRESSION  --
!
                        if (.not.lmax .and. .not.lmin) then
                            ilig=(icomp2+ndim)/6
                            ires=(icomp2+ndim)-ilig*6
                            fmt1 = ' '
                            fmt2 = ' '
                            if (ires .ne. 0) then
                                fmt1 = '(9X, 6(1X, '//forcmp// '), 30(/, 9X, 6(1X, '//forcmp//')))'
                                fmt2 = '(1X, A8, 6(1X, '//format// '),30(/, 9X, 6(1X, '&
                                       //format//')))'
                            else if (ires.eq.0.and.ilig.eq.1) then
                                fmt1 = '(9X,6(1X,'//forcmp//'))'
                                fmt2 = '(1X,A8,6(1X,'//format//'))'
                            else
                                write(fmt1,'(A,A8,A,I2,A,A8,A)')&
     &                               '(9X,6(1X,',forcmp,'),',&
     &                               (ilig-1),'(/,9X,6(1X,',forcmp,')))'
                                write(fmt2,'(A,A10,A,I2,A,A10,A)')&
                                '(1X,A8,6(1X,',format,'),', (ilig-1),&
                                '(/,9X,6(1X,',format,')))'
                            endif
                            if (lsup .or. linf) then
                                if (limpr) then
                                    write(ifi,'(A,I2,A)') nomel(iel)
                                    limpr=.false.
                                endif
                            endif
                            if (ndim .eq. 0) then
                                write(ifi,fmt1) (zk16(inop-1+i)(1:11),&
                                i=1,icomp2)
                                write(ifi,fmt2) nomno,(zr(irval-1+&
                                icmp), icmp=1,icomp2)
                                write(ifi,fmt2) kbid,(zr(icval-1+icmp)&
                                , icmp=1,icomp2)
                            else
                                write(ifi,fmt1) (nomcor(i),i=1,ndim),&
                                (zk16(inop-1+i)(1:11),i=1,icomp2)
                                write(ifi,fmt2) nomno,(coor((nuno-1)*&
                                3+i), i=1,ndim),(zr(irval-1+icmp),&
                                icmp=1,icomp2)
                                write(ifi,fmt2) kbid,(coor((nuno-1)*3+&
                                i), i=1,ndim),(zr(icval-1+icmp),icmp=&
                                1,icomp2)
                            endif
                        endif
                        nbcpt=icomp2
                    else
                        if (.not.lmax .and. .not.lmin) then
                            if (ndim .eq. 0) then
                                write(ifi,fmv) nomno,(zr(irval-1+icmp)&
                                , icmp=1,icoef2*ncmp2)
                                write(ifi,fmv) kbid,(zr(icval-1+icmp),&
                                icmp=1,icoef2*ncmp2)
                            else
                                write(ifi,fmv) nomno,(coor((nuno-1)*3+&
                                i), i=1,ndim),(zr(irval-1+icmp),&
                                icmp=1,icoef2*ncmp2)
                                write(ifi,fmv) kbid,(coor((nuno-1)*3+&
                                i), i=1,ndim),(zr(icval-1+icmp),&
                                icmp=1,icoef2*ncmp2)
                            endif
                        endif
                        nbcpt=icoef2*ncmp2
                    endif
!
! -- RECHERCHE DE LA VALEUR MAXIMALE ---
!
                    if (lmax) then
                        do 103 i = 1, nbcpt
                            if (lsup .or. linf) then
                                iadr=(zi(ipo2-1+i)-1)*icoef2+zi(&
                                icoe-1+i)
                            else
                                iadr=(zi(iposg-1+i)-1)*icoef2+zi(&
                                icoe-1+i)
                            endif
                            if (zr(irmax-1+iadr) .eq. rundf) then
                                zr(irmax-1+iadr) = zr(irval-1+i)
                                zr(icmax-1+iadr) = zr(icval-1+i)
                                zk8(inmax-1+iadr) = nomel(iel)
                                zi(ivmax-1+iadr) = 1
                            else
                                valmax=sqrt(zr(irmax-1+iadr)**2 +&
                                zr(icmax-1+iadr)**2)
                                value=sqrt(zr(irval-1+i)**2 + zr(&
                                icval-1+i)**2)
                                if (value .gt. valmax) then
                                    zr(irmax-1+iadr)= zr(irval-1+i)
                                    zr(icmax-1+iadr)= zr(icval-1+i)
                                    zk8(inmax-1+iadr) = nomel(iel)
                                    zi(ivmax-1+iadr) = 1
                                else if (value.eq.valmax) then
                                    zi(ivmax-1+iadr)=zi(ivmax-1+iadr)+&
                                    1
                                endif
                            endif
103                      continue
                    endif
!
! -- RECHERCHE DE LA VALEURE MINIMALE ---
!
                    if (lmin) then
                        do 104 i = 1, nbcpt
                            if (lsup .or. linf) then
                                iadr=(zi(ipo2-1+i)-1)*icoef2+zi(&
                                icoe-1+i)
                            else
                                iadr=(zi(iposg-1+i)-1)*icoef2+zi(&
                                icoe-1+i)
                            endif
                            if (zr(irmin-1+iadr) .eq. rundf) then
                                zr(irmin-1+iadr) = zr(irval-1+i)
                                zr(icmin-1+iadr) = zr(icval-1+i)
                                zk8(inmin-1+iadr) = nomel(iel)
                                zi(ivmin-1+iadr) = 1
                            else
                                valmin=sqrt(zr(irmin-1+iadr)**2 +&
                                zr(icmin-1+iadr)**2)
                                value=sqrt(zr(irval-1+i)**2 + zr(&
                                icval-1+i)**2)
                                if (value .lt. valmin) then
                                    zr(irmin-1+iadr)= zr(irval-1+i)
                                    zr(icmin-1+iadr)= zr(icval-1+i)
                                    zk8(inmin-1+iadr) = nomel(iel)
                                    zi(ivmin-1+iadr) = 1
                                else if (value.eq.valmin) then
                                    zi(ivmin-1+iadr)=zi(ivmin-1+iadr)+&
                                    1
                                endif
                            endif
104                      continue
                    endif
18              continue
17          continue
        endif
12  end do
    write (ifi,*) ' '
!
! --- IMPRESSION DE LA VALEUR MAXIMALE ---
!
    if (lmax) then
        do 95 i = 1, ncmpmx*icoef2
            if (zr(irmax-1+i) .ne. rundf) then
                form1 = '(1X,3A,'//format//',1X,'//format//',A,I4,2A)'
                write(ifi,form1)'LA VALEUR MAXIMALE DE ',zk16(inot-1+&
                i), ' EST ',zr(irmax-1+i),zr(icmax-1+i), ' EN ',zi(&
                ivmax-1+i),' MAILLE(S) : ',zk8(inmax-1+i)
            endif
95      continue
    endif
!
! --- IMPRESSION DE LA VALEUR MINIMALE ---
!
    if (lmin) then
        do 96 i = 1, ncmpmx*icoef2
            if (zr(irmin-1+i) .ne. rundf) then
                form1 = '(1X,3A,'//format//',1X,'//format//',A,I4,2A)'
                write(ifi,form1)'LA VALEUR MINIMALE DE ',zk16(inot-1+&
                i), ' EST ',zr(irmin-1+i),zr(icmin-1+i), ' EN ',zi(&
                ivmin-1+i),' MAILLE(S) : ',zk8(inmin-1+i)
            endif
96      continue
    endif
!
    call jedetr('&&IRCECL.NCMT')
    call jedetr('&&IRCECL.MAXR')
    call jedetr('&&IRCECL.MAXC')
    call jedetr('&&IRCECL.MAIMAX')
    call jedetr('&&IRCECL.NBVMAX')
    call jedetr('&&IRCECL.MINR')
    call jedetr('&&IRCECL.MINC')
    call jedetr('&&IRCECL.MAIMIN')
    call jedetr('&&IRCECL.NBVMIN')
    call jedetr('&&IRCECL.ENT_COD')
    call jedetr('&&IRCECL.POSG')
    call jedetr('&&IRCECL.POSV')
    call jedetr('&&IRCECL.COEF')
    call jedetr('&&IRCECL.NCMP')
    call jedetr('&&IRCECL.NCPP')
    call jedetr('&&IRCECL.PO2')
    call jedetr('&&IRCECL.VALR')
    call jedetr('&&IRCECL.VALC')
!
9999  continue
    call jedema()
end subroutine

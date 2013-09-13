subroutine op0153()
    implicit none
!-----------------------------------------------------------------------
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
!
!     OPERATEUR  "POST_USURE"
!
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/r8prem.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/motubn.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbextb.h"
#include "asterfort/tbexv1.h"
#include "asterfort/tbexve.h"
#include "asterfort/tbliva.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/usupru.h"
#include "asterfort/usupus.h"
#include "asterfort/usuvu2.h"
#include "asterfort/usuvus.h"
#include "asterfort/wkvect.h"
!-----------------------------------------------------------------------
    integer :: i, ibid, idangt, idcotu, idvcob, idvctu, ifires
    integer :: indic, info, iobst, ipoupr, ipourp, iprfuo, iprfut
    integer :: ipus, ire1, ire2, iret, itube, ivuso
    integer :: ivusob, ivust, ivustu, jfn, jins2, jinst, jinst3
    integer :: jinst5, jprut, jsect, jusuo, jusut, jvg, k
    integer :: n0, n1, n5, na, nbinst, nbpair, nbpar
    integer :: nbpar2, nbpmr, nbpt, nbsec2, nbsect, nbtota, nbv
    integer :: nbvpu, ni1, nis, npu, ntn
    real(kind=8) :: coinst, haut, puusur, rayoo, rayot
!
!-----------------------------------------------------------------------
    parameter    ( nbpar = 16, nbpar2 = 12, nbpmr = 5 )
    real(kind=8) :: pmoye, insdeb, epsil, dinst
    character(len=8) :: k8b
    character(len=8) :: typar(nbpar), typpmr(nbpmr)
    real(kind=8) :: valer(nbpar)
    character(len=16) :: nopar(nbpar), nompmr(nbpmr), nopar2(nbpar2)
    character(len=16) :: concep, nomcmd, valek(2)
    character(len=19) :: resu, linst, kforn, kvgli
    character(len=19) :: tabpus, nomta, newtab
    character(len=24) :: type, valk(2)
    complex(kind=8) :: c16b
    data nopar  / 'PUIS_USUR_GLOBAL' ,&
     &              'INST' , 'DUREE' , 'ORIG_INST' ,&
     &              'V_USUR_TUBE' , 'V_USUR_OBST' , 'P_USUR_TUBE' ,&
     &              'SECTEUR' , 'ANGLE_DEBUT' , 'ANGLE_FIN' ,&
     &              'V_USUR_TUBE_SECT' , 'V_USUR_OBST_SECT' ,&
     &              'P_USUR_TUBE_SECT' , 'P_USUR_OBST_SECT' ,&
     &              'V_USUR_TUBE_CUMU' , 'V_USUR_OBST_CUMU' /
    data typar  /'R','R','R','R','R','R','R','I','R','R','R','R','R',&
     &             'R','R','R'/
    data nopar2 / 'INST' , 'DUREE' , 'ORIG_INST' ,&
     &              'SECTEUR' , 'ANGLE_DEBUT' , 'ANGLE_FIN' ,&
     &              'V_USUR_TUBE_SECT' , 'V_USUR_OBST_SECT' ,&
     &              'P_USUR_TUBE_SECT' , 'P_USUR_OBST_SECT' ,&
     &              'V_USUR_TUBE_CUMU' , 'V_USUR_OBST_CUMU' /
    data nompmr /'PUIS_USUR_GLOBAL' ,&
     &             'INST','V_USUR_TUBE','V_USUR_OBST','P_USUR_TUBE'/
    data typpmr /'R','R','R','R','R'/
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    indic = 0
    ifires = iunifi('RESULTAT')
    kforn = '&&OP0153.FORC_N'
    kvgli = '&&OP0153.VITE_G'
!
    call getres(resu, concep, nomcmd)
!
!     ------------------------------------------------------------------
!            REMPLACEMENT DU TUBE PERCE PAR UN TUBE NEUF
!     ------------------------------------------------------------------
!
    dinst = 0.d0
    call getvtx(' ', 'TUBE_NEUF', scal=k8b, nbret=ntn)
    if (ntn .ne. 0) then
        call exisd('TABLE', resu, iret)
        if (iret .eq. 0) then
            call u2mess('F', 'PREPOST4_7')
        endif
        call getvid(' ', 'TABL_USURE', scal=k8b, nbret=n1)
        if (k8b .ne. resu(1:8)) then
            call u2mess('F', 'PREPOST4_7')
        endif
        call getvr8(' ', 'INST', scal=dinst, nbret=nis)
        if (nis .eq. 0) then
            call tbexv1(resu, 'INST', '&&OP0153.INST', 'V', nbv,&
                        k8b)
            call jeveuo('&&OP0153.INST', 'L', jinst)
            dinst = zr(jinst+nbv-1)
        endif
        call tbexv1(resu, 'SECTEUR', '&&OP0153.SECT', 'V', nbv,&
                    k8b)
        call jeveuo('&&OP0153.SECT', 'L', jsect)
        nbsect = zi(jsect+nbv-1)
        call jedetr('&&OP0153.SECT')
        call motubn(resu, dinst, nbsect)
        goto 888
    endif
!
!     ------------------------------------------------------------------
    call getvis(' ', 'INFO', scal=info, nbret=n0)
    if (info .gt. 1) then
        write(ifires,1000)
        write(ifires,*)
        write(ifires,*) resu
    endif
!
!     --- CALCUL DE LA PUISSANCE D'USURE ---
    call usupus(puusur, kforn, kvgli, nbpt)
    call jeexin(kforn, iret)
    jfn=1
    jvg=1
    if (iret .gt. 0) then
        call jeveuo(kforn, 'E', jfn)
        call jeveuo(kvgli, 'E', jvg)
    endif
!
!     --- RECUPERATION DES INSTANTS DE CALCUL ---
    call getvr8(' ', 'INST', nbval=0, nbret=ni1)
    if (ni1 .ne. 0) then
        nbinst = -ni1
        call wkvect('&&OP0153.INSTANT', 'V V R', nbinst, jinst)
        call getvr8(' ', 'INST', nbval=nbinst, vect=zr(jinst), nbret=n1)
    else
        call getvid(' ', 'LIST_INST', scal=linst, nbret=n1)
        call jelira(linst//'.VALE', 'LONUTI', nbinst)
        call jeveuo(linst//'.VALE', 'L', jinst)
    endif
    call wkvect('&&OP0153.INSTAN2', 'V V R', nbinst, jins2)
    do 10 i = 0, nbinst-1
        zr(jins2+i) = zr(jinst+i)
10  end do
    call getvr8(' ', 'COEF_INST', scal=coinst, nbret=n1)
    if (n1 .ne. 0) then
        do 12 i = 0, nbinst-1
            zr(jins2+i) = zr(jins2+i) * coinst
12      continue
    endif
!
    call wkvect('&&OP0153.USURE_TUBE', 'V V R', nbinst, jusut)
    call wkvect('&&OP0153.USURE_OBST', 'V V R', nbinst, jusuo)
    call wkvect('&&OP0153.PRONF_TUBE', 'V V R', nbinst, jprut)
!
    call getfac('SECTEUR', nbsect)
    if (nbsect .ne. 0) then
        nbpair = nbsect+1
        nbtota = nbsect*nbinst
        call wkvect('&&OP0153.ANGT', 'V V R', nbpair, idangt)
        call wkvect('&&OP0153.VUSTUB', 'V V R', nbtota, ivustu)
        call wkvect('&&OP0153.VUSOB', 'V V R', nbtota, ivusob)
        call wkvect('&&OP0153.PRFUST', 'V V R', nbtota, iprfut)
        call wkvect('&&OP0153.PRFUSO', 'V V R', nbtota, iprfuo)
        call wkvect('&&OP0153.VUST', 'V V R', nbsect, ivust)
        call wkvect('&&OP0153.VUSO', 'V V R', nbsect, ivuso)
        call wkvect('&&OP0153.PUS', 'V V R', nbsect, ipus)
        call wkvect('&&OP0153.POUPRE', 'V V R', nbsect, ipoupr)
        call wkvect('&&OP0153.POURPU', 'V V R', nbsect, ipourp)
        call wkvect('&&OP0153.VCTU', 'V V R', nbsect, idvctu)
        call wkvect('&&OP0153.VCOB', 'V V R', nbsect, idvcob)
        call wkvect('&&OP0153.COTU', 'V V K16', nbsect, idcotu)
        epsil = 1.d-4
        do 14 i = 1, nbsect
            if (i .eq. 1) then
                call getvr8('SECTEUR', 'ANGL_INIT', iocc=1, scal=zr(idangt), nbret=na)
!
!              LES ANGLES SONT CROISSANTS ENTRE -180. ET +180. :
!              -----------------------------------------------
                if ((zr(idangt).lt.(-180.d0-epsil)) .or. (zr(idangt) .gt.(-180.d0+epsil))) then
                    call u2mess('F', 'PREPOST4_8')
                endif
            endif
            call getvr8('SECTEUR', 'ANGL_FIN', iocc=i, scal=zr(idangt+i), nbret=na)
            if (zr(idangt+i) .lt. zr(idangt+i-1)) then
                call u2mess('F', 'PREPOST4_9')
            endif
            if (i .eq. nbsect) then
                if ((zr(idangt+i).lt.(180.d0-epsil)) .or. (zr(idangt+ i).gt.(180.d0+epsil))) then
                    call u2mess('F', 'PREPOST4_10')
                endif
            endif
            call getvr8('SECTEUR', 'COEF_USUR_MOBILE', iocc=i, scal=zr(idvctu+i-1), nbret=n5)
            call getvr8('SECTEUR', 'COEF_USUR_OBST', iocc=i, scal=zr( idvcob+i-1), nbret=n5)
            call getvtx('SECTEUR', 'CONTACT', iocc=i, scal=zk16(idcotu+i- 1), nbret=n5)
14      continue
    else
        indic = 1
    endif
!
    if (puusur .le. r8prem()) goto 777
!
!     --- CALCUL DU VOLUME D'USURE TUBE ---
    itube = 1
!
    if (indic .eq. 0) then
        call usuvu2(puusur, zr(jusut), nbinst, zr(jins2), itube,&
                    nbpt, nbsect, zr(idvctu), zr(idangt), zr(jfn),&
                    zr(jvg), iret, zr(ivustu), zr(ivusob), zr(ipus),&
                    pmoye, zr(ipourp), zr( ipoupr))
    else if (indic.eq.1) then
        call usuvus(puusur, zr(jusut), nbinst, zr(jins2), itube,&
                    nbpt, zr(jfn), zr(jvg), iret)
    endif
    if (iret .ne. 0) goto 9999
!
!     --- CALCUL DU VOLUME D'USURE OBSTABLE ---
    iobst = 2
!
    if (indic .eq. 0) then
        call usuvu2(puusur, zr(jusuo), nbinst, zr(jins2), iobst,&
                    nbpt, nbsect, zr(idvcob), zr(idangt), zr(jfn),&
                    zr(jvg), iret, zr(ivustu), zr(ivusob), zr(ipus),&
                    pmoye, zr(ipourp), zr( ipoupr))
    else if (indic.eq.1) then
        call usuvus(puusur, zr(jusuo), nbinst, zr(jins2), iobst,&
                    nbpt, zr(jfn), zr(jvg), iret)
    endif
    if (iret .ne. 0) goto 9999
!
    if (indic .eq. 0) then
        call getvr8(' ', 'LARGEUR_OBST', scal=haut, nbret=n1)
        if (n1 .le. 0) then
            haut=0.011d0
        endif
        if (info .gt. 1) write(ifires,1130)
        call getvr8(' ', 'RAYON_MOBILE', scal=rayot, nbret=n1)
        if (n1 .eq. 0) then
            call u2mess('F', 'PREPOST4_11')
        endif
        do 24 i = 1, nbsect
            do 22 k = 1, nbinst
                zr(iprfut+(k-1)*nbsect+i-1) = rayot - sqrt(&
                                              rayot* rayot-2.d0*zr(&
                                              ivustu+(k-1)*nbsect+i-1)/ (haut*(zr( idangt+i)-zr(i&
                                              &dangt+i-1))&
                                              )&
                                              )
22          continue
24      continue
        call getvr8(' ', 'RAYON_OBST', scal=rayoo, nbret=n1)
        if (n1 .eq. 0) then
            call u2mess('F', 'PREPOST4_12')
        endif
        do 20 i = 1, nbsect
            do 23 k = 1, nbinst
                zr(iprfuo+(k-1)*nbsect+i-1) = rayoo - sqrt(&
                                              rayoo* rayoo-2.d0*zr(&
                                              ivusob+(k-1)*nbsect+i-1)/ (haut*(zr( idangt+i)-zr(i&
                                              &dangt+i-1))&
                                              )&
                                              )
23          continue
20      continue
    endif
!
!      --- IMPRESSIONS DES RESULTATS ---
!
!
    if (indic .ne. 0) goto 666
    do 25 i = 1, nbsect
        if (info .gt. 1) then
            write(ifires,*)
            write(ifires,*)
            write(ifires,1090) 'SECTEUR : ',zr(idangt+i-1),' / ',&
            zr(idangt+i)
            write(ifires,*)
            write(ifires,1120) 'TYPE DE CONTACT     ',':',&
     &    zk16(idcotu+i-1)
            write(ifires,1040) 'COEF USURE TUBE     ',':',&
     &    zr(idvctu+i-1)
            write(ifires,1040) 'COEF USURE OBSTACLE ',':', zr(idvcob+&
            i-1)
            write(ifires,1030) 'PRESENCE DU CRAYON  ',':',&
     &                       zr(ipoupr+i-1)*100.d0,'%'
            write(ifires,1060) 'PUISSANCE D USURE   ',':',&
     &    zr(ipus+i-1),'W'
            write(ifires,1030) '% PU DANS CE SECTEUR',':', zr(ipourp+&
            i-1),'%'
            write(ifires,*)
            write(ifires,1010) 'ANNEES','V_USUR_TUBE','V_USUR_OBST',&
            'P_USUR_TUBE','P_USUR_OBST'
        endif
        do 27 k = 1, nbinst
            if (info .gt. 1) write(ifires, 1080) (zr(jins2+k-1) / coinst),&
                             zr(ivustu+(k-1)*nbsect+i-1), zr(ivusob+(k-1)*nbsect+i-1),&
                             zr(iprfut+(k-1)*nbsect+i-1), zr(iprfuo+(k-1)*nbsect+i-1)
27      continue
25  end do
666  continue
!     --- CALCUL DE PROFONDEUR D'USURE ---
    if (indic .eq. 1) call usupru(zr(jusut), zr(jusuo), nbinst, zr(jprut))
777  continue
!
    if (indic .eq. 1) then
!        --- CREATION DE LA TABLE ---
        call tbcrsd(resu, 'G')
        call tbajpa(resu, nbpmr, nompmr, typpmr)
        call tbajli(resu, 1, 'PUIS_USUR_GLOBAL', ibid, puusur,&
                    c16b, k8b, 0)
        do 30 k = 1, nbinst
            valer(1) = zr(jins2+k-1) / coinst
            valer(2) = zr(jusut+k-1)
            valer(3) = zr(jusuo+k-1)
            valer(4) = zr(jprut+k-1)
            call tbajli(resu, 4, nompmr(2), ibid, valer,&
                        c16b, k8b, 0)
30      continue
        goto 888
    endif
!
!     REPRISE EVENTUELLE ET STOCKAGE DE LA TABLE POST_USURE :
!     -----------------------------------------------------
!
    call getvid('ETAT_INIT', 'TABL_USURE', iocc=1, scal=tabpus, nbret=npu)
    if (npu .eq. 0) then
        dinst = 0.d0
        call tbcrsd(resu, 'G')
        call tbajpa(resu, nbpar, nopar, typar)
    else
        if (tabpus .ne. resu) then
            call u2mess('F', 'PREPOST4_13')
        endif
!   ON REPREND UNE TABLE EXISTANTE
        nomta = tabpus
        call tbexp2(nomta, 'INST')
        call tbexp2(nomta, 'SECTEUR')
        call tbexp2(nomta, 'V_USUR_OBST_CUMU')
        call tbexp2(nomta, 'V_USUR_TUBE_CUMU')
        call tbexve(nomta, 'INST', '&&OP0153.INS3', 'V', nbvpu,&
                    type)
        call jeveuo('&&OP0153.INS3', 'L', jinst3)
        insdeb = zr(jinst3+nbvpu-1)
        call tbexve(nomta, 'SECTEUR', '&&OP0153.SECT', 'V', nbvpu,&
                    type)
        call jeveuo('&&OP0153.SECT', 'L', jsect)
        nbsec2 = zi(jsect+nbvpu-1)
        if (nbsec2 .ne. nbsect) then
            call u2mess('F', 'PREPOST4_14')
        endif
        call getvr8('ETAT_INIT', 'INST_INIT', iocc=1, scal=dinst, nbret=nis)
        if (nis .eq. 0) then
            dinst = insdeb
        else if (dinst.gt.insdeb) then
            dinst = insdeb
        else
            newtab = '&&OP0153.NEWTAB'
            call tbextb(nomta, 'V', newtab, 1, 'INST',&
                        'LE', ibid, dinst, c16b, k8b,&
                        1.d-03, 'RELA', iret)
            if (iret .eq. 10) then
                valk(1) = 'INST'
                valk(2) = nomta
                call u2mesk('F', 'UTILITAI7_1', 2, valk)
            else if (iret .eq. 20) then
                valk(1) = nomta
                valk(2) = 'INST'
                call u2mesk('F', 'UTILITAI7_3', 2, valk)
            endif
            call detrsd('TABLE', nomta)
            call tbextb(newtab, 'G', nomta, 1, 'INST',&
                        'LE', ibid, dinst, c16b, k8b,&
                        1.d-03, 'RELA', iret)
            if (iret .eq. 10) then
                valk(1) = 'INST'
                valk(2) = newtab
                call u2mesk('F', 'UTILITAI7_1', 2, valk)
            else if (iret .eq. 20) then
                valk(1) = newtab
                valk(2) = 'INST'
                call u2mesk('F', 'UTILITAI7_3', 2, valk)
            endif
            call tbexve(nomta, 'INST', '&&OP0153.INS5', 'V', nbvpu,&
                        type)
            call jeveuo('&&OP0153.INS5', 'L', jinst5)
            dinst = zr(jinst5+nbvpu-1)
        endif
!
!        DETERMINATION PAR SECTEUR DES VOLUS PAR TUBE ET OBST A DINST
!        ------------------------------------------------------------
!
        valek(1) = 'INST'
        valek(2) = 'SECTEUR'
        do 1 i = 1, nbsect
            call tbliva(nomta, 2, valek, i, dinst,&
                        c16b, k8b, 'RELA', 1.d-03, 'V_USUR_TUBE_CUMU',&
                        k8b, ibid, zr(ivust+i-1), c16b, k8b,&
                        ire1)
            call tbliva(nomta, 2, valek, i, dinst,&
                        c16b, k8b, 'RELA', 1.d-03, 'V_USUR_OBST_CUMU',&
                        k8b, ibid, zr(ivuso+i-1), c16b, k8b,&
                        ire2)
            if ((ire1+ire2) .gt. 0) then
                call u2mesk('F', 'PREPOST4_15', 1, nomta)
            endif
 1      continue
    endif
!
    call tbajli(resu, 1, 'PUIS_USUR_GLOBAL', ibid, puusur,&
                c16b, k8b, 0)
!
    do 26 k = 1, nbinst
!        -INST-
        valer(1) = zr(jins2+k-1) / coinst + dinst
!        -DUREE-
        valer(2) = zr(jins2+k-1) / coinst
!        -ORIG_INST-
        valer(3) = dinst
!        -V_USUR_TUBE-
        valer(4) = zr(jusut+k-1)
!        -V_USUR_OBST-
        valer(5) = zr(jusuo+k-1)
!        -P_USUR_TUBE-
        valer(6) = zr(jprut+k-1)
        call tbajli(resu, 6, nopar(2), ibid, valer,&
                    c16b, k8b, 0)
        do 28 i = 1, nbsect
!           -ANGLE_DEBUT-
            valer(4) = zr(idangt+i-1)
!           -ANGLE_FIN-
            valer(5) = zr(idangt+i)
!           -V_USUR_TUBE_SECT-
            valer(6) = zr(ivustu+(k-1)*nbsect+i-1)
!           -V_USUR_OBST_SECT-
            valer(7) = zr(ivusob+(k-1)*nbsect+i-1)
!           -P_USUR_TUBE_SECT-
            valer(8) = zr(iprfut+(k-1)*nbsect+i-1)
!           -P_USUR_OBST_SECT-
            valer(9) = zr(iprfuo+(k-1)*nbsect+i-1)
!           -V_USUR_TUBE_CUMU-
            valer(10) = zr(ivustu+(k-1)*nbsect+i-1) + zr(ivust+i-1)
!           -V_USUR_OBST_CUMU-
            valer(11) = zr(ivusob+(k-1)*nbsect+i-1) + zr(ivuso+i-1)
            call tbajli(resu, nbpar2, nopar2, i, valer,&
                        c16b, k8b, 0)
28      continue
26  end do
    if (nbsect .ne. 0 .and. info .gt. 1) then
        write(ifires,*)
        write(ifires,*) 'PUISSANCE D USURE MOYENNE'
        write(ifires,1020) pmoye,'W'
    endif
!
888  continue
!
    call titre()
!
    1000 format(/,80('-'))
    1010 format(a11,2x,a15,2x,a15,2x,a15,2x,a15)
    1020 format(1pe12.5,1x,a1)
    1030 format(a20,1x,a1,1x,f6.2,1x,a1)
    1040 format(a20,1x,a1,1x,1pe11.4)
    1060 format(a20,1x,a1,1x,1pe12.5,1x,a1)
    1080 format(1pe12.5,2x,1pe16.9,2x,1pe16.9,2x,1pe16.9,2x,1pe16.9)
    1090 format(a10,1x,f7.2,a3,f7.2)
    1120 format(a20,1x,a1,1x,a14)
    1130 format(&
     &'LES PROFONDEURS USEES PAR SECTEUR SONT DES APPROXIMATIONS')
!
9999  continue
    call jedema()
end subroutine

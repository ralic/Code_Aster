subroutine fluimp(itypfl, nivpar, nivdef, melflu, typflu,&
                  nuor, freq, freqi, nbm, vite,&
                  npv, carac, calcul, amoc)
! aslint: disable=W1501
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
!-----------------------------------------------------------------------
! IMPRESSION DANS LE FICHIER RESULTAT DES PARAMETRES DE COUPLAGE
! FLUIDE-STRUCTURE (FREQ,AMOR) ET/OU DES DEFORMEES MODALES
! APPELANT : FLUST1, FLUST2, FLUST3, FLUST4
!-----------------------------------------------------------------------
!  IN : ITYPLF : INDICE CARACTERISANT LE TYPE DE LA CONFIGURATION
!                ETUDIEE
!  IN : NIVPAR : NIVEAU D'IMPRESSION DES PARAMETRES DU COUPLAGE
!                (FREQ,AMOR)
!  IN : NIVDEF : NIVEAU D'IMPRESSION DES DEFORMEES MODALES
!  IN : MELFLU : NOM UTILISATEUR DU CONCEPT MELASFLU
!  IN : NUOR   : LISTE DES NUMEROS D'ORDRE DES MODES SELECTIONNES POUR
!                LE COUPLAGE
!  IN : FREQ   : LISTE DES FREQUENCES ET AMORTISSEMENTS REDUITS MODAUX
!                PERTURBES PAR L'ECOULEMENT
!  IN : FREQI  : LISTE DES FREQUENCES MODALES INITIALES
!  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
!  IN : VITE   : LISTE DES VITESSES D'ECOULEMENT ETUDIEES
!  IN : NPV    : NOMBRE DE VITESSES D'ECOULEMENT
!  IN : CARAC   : DIAMETRE HYDRAULIQUE ET EPAISSEUR
!-----------------------------------------------------------------------
!
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/codree.h"
#include "asterfort/irdepl.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: itypfl, nivpar, nivdef, nbm, npv, nuor(nbm), jvcn, jven
    integer :: nive, nbval, lfsvi, pas, jconn, jrap
    character(len=19) :: melflu
    character(len=8) :: typflu
    real(kind=8) :: carac(2), freq(2*nbm*npv), freqi(*), vite(npv), amoc(*)
    real(kind=8) :: vrmin, vrmax, vrmin1, vrmin2, vrmax1, vrmax2
    real(kind=8) :: vmoy, vmoyto, reduit, rappor, rappo2
!
    integer :: jtrav1, jtrav2, jtrav3, jtrav4, jvit1, jvit2, jzone
    integer :: jtr1, jtr2, jvrzo, lfsvr, jcste
    integer :: lprofv, lnoe, iret, modul, modul2
    character(len=3) :: i3
    character(len=8) :: k8bid, nomsym, nomcmp(6), formar, numzo, nbpzon
    character(len=8) :: xl1, xl2, xl3
    character(len=13) :: xcod, xvred, xfreq1, xamor, xbmin, xbmax
    character(len=13) :: xvmin, xvmax, xvmoy
    character(len=30) :: cham30
    character(len=19) :: cham19
    character(len=24) :: nom1, nom2, fsvi, ccste
    character(len=100) :: chav11, chav12, chav13, chav21, chav22, chav23
    character(len=100) :: chav31, chav32, chav33, chav34
    character(len=100) :: chazp1, chazv1, chazp2, chazv2, chazp3, chazv3
    character(len=100) :: chazp4, chazv4, chazp5, chazv5, chazp6, chazv6
    character(len=100) :: chazp7, chazv7, chav40, chaz40
    character(len=255) :: ctrav1, ctrav2, ctrav3
    logical :: lcor, lsup, linf, lmin, lmax, lresu, calcul(2)
!
!-----------------------------------------------------------------------
    integer :: i, ibid, ifr, ik, im, imod, ind
    integer :: iv, j, jprofv, jtrav5, k, l1
    integer :: l2, l3, n1, n2, npasv, nzone
!
    real(kind=8) :: amor1, bmax, bmin, dif1, freq1, rbid, vred
!
!-----------------------------------------------------------------------
    data           nomcmp /'DX      ','DY      ','DZ      ',&
     &                     'DRX     ','DRY     ','DRZ     '/
!
!-----------------------------------------------------------------------
!
    call jemarq()
    ifr = iunifi('RESULTAT')
!
    nom1 = '&&COEFAM.CDR2'
    nom2 = '&&COEFRA.CKR2'
    cham30='******************************'
!
!
    cham19(1:13) = melflu(1:8)//'.C01.'
    nomsym = 'DEPL_R  '
    formar = '1PE12.5'
    nive = 3
!
    npasv = npv
!
    lcor = .false.
    lsup = .false.
    linf = .false.
    lmin = .false.
    lmax = .false.
    lresu = .false.
!
!
!     VERIFICATION DE LA COHERENCE DES VITESSES REDUITES
!     ENTRE LES FICHIERS .70 ET .71 - OPTION FAISCEAU-TRANS
!
    if (itypfl .eq. 1) then
        call jeexin(nom1, iret)
        if (iret .ne. 0) then
            call jeveuo(nom1, 'L', jvit1)
            call jeveuo(nom2, 'L', jvit2)
            vrmin1 = zr(jvit1-1+1)
            vrmax1 = zr(jvit1-1+2)
            vrmin2 = zr(jvit2-1+1)
            vrmax2 = zr(jvit2-1+2)
            if ((abs(vrmin1-vrmin2)) .gt. 1.0d-04 .or. (abs(vrmax1- vrmax2)) .gt. 1.0d-04) then
                call u2mess('F', 'ALGELINE_42')
            endif
        endif
!
    endif
!
    if (nivpar .eq. 1) then
        if (calcul(1)) then
            write (ifr,*)
            write (ifr,*) '==============================================='
            write (ifr,*)
            write (ifr,*) ' RESULTAT MODULE COUPLAGE FLUIDE-STRUCTURE'
            write (ifr,*)
            write (ifr,*) 'EVOLUTION DE LA FREQUENCE ET DE L AMORTISSEMENT'
            write (ifr,*) '   EN FONCTION DE LA VITESSE DE L ECOULEMENT'
            write (ifr,*)
            write (ifr,*) '==============================================='
            write (ifr,*)
!
            call jeexin('&&FLUST1.TEMP.PROFV', iret)
            if (iret .ne. 0) then
                call jelira('&&FLUST1.TEMP.PROFV', 'LONMAX', lprofv)
                lnoe = (lprofv-1)/2
                call jeveuo('&&FLUST1.TEMP.PROFV', 'L', jprofv)
                vmoyto = zr(jprofv-1+lprofv)
                write (ifr,1001) vmoyto
                write (ifr,*)
            endif
        endif
        if (itypfl .eq. 1) then
            call jeveuo('&&MDCONF.TEMPO', 'L', jzone)
            nzone = zi(jzone-1+1)
            if (calcul(2)) then
                call jeveuo(melflu(1:8)//'.VEN', 'L', jven)
                call jeveuo(melflu(1:8)//'.VCN', 'L', jvcn)
                call jeveuo(melflu(1:8)//'.MASS', 'L', jconn)
                call jeveuo(melflu(1:8)//'.RAP', 'L', jrap)
!
                call jeveuo(typflu//'           .FSVR', 'L', lfsvr)
                fsvi = typflu//'           .FSVI'
                call jeveuo(fsvi, 'L', lfsvi)
                nbval=1
                ctrav2=' *'
                do 50 i = 1, nzone
                    nbval=nbval*zi(lfsvi+1+nzone+i)
                    ctrav2((2+(30*(i-1))):(2+(30*i)))=cham30
50              continue
!
                do 60 i = 1, 180
                    ctrav2((1+(30*nzone)+i):(1+(30*nzone)+i))='*'
60              continue
                ctrav2((1+(30*nzone)+181):(1+(30*nzone)+181))='*'
                call wkvect('&&FLUIMP.CSTE', 'V V R', nzone, jcste)
            endif
        endif
!
!
        do 10 im = 1, nbm
            imod = nuor(im)
            if (calcul(1)) then
                write (ifr,1002) imod,freqi(imod)
                write (ifr,*) ' ------------------------------------------'
                write (ifr,*)
!
! ---     ECRITURE DE L'EN-TETE DU TABLEAU POUR CHAQUE MODE
                chav11 = '****************'
                chav12 = '                '
                chav13 = '               *'
                chav21 = '    VITESSE    *'
                chav22 = '   FREQUENCE   *'
                chav23 = ' AMORTISSEMENT *'
                chav31 = '    GAP(M/S)   *'
                chav32 = '    REDUITE    *'
                chav33 = '     (HZ)      *'
                chav34 = '       %       *'
            endif
            if (itypfl .eq. 1) then
                if (calcul(1)) then
!
                    chazp1 = '****************************'
                    chazp2 = '                            '
                    chazp3 = ' VITESSE MOY : '
                    chazp4 = '----------------------------'
                    chazp5 = ' NB POINTS DE LA ZONE :'
                    chazp6 = '     DONT HORS PLAGE :      '
                    chazp7 = ' <VREDMIN   TOT    >VREDMAX '
!
                    chazv1 = '*********************************'
                    chazv2 = '  ZONE  '
                    chazv3 = '|    PLAGE DE VITESSE REDUITE   *'
                    chazv4 = '|-------------------------------*'
                    chazv5 = '|    VREDMIN    |    VREDMAX    *'
                    chazv6 = '| '
                    chazv7 = '|               |               *'
!
                    call wkvect('&&FLUIMP.TRAV1', 'V V K80', nzone, jtrav1)
                    call wkvect('&&FLUIMP.TRAV2', 'V V K80', nzone, jtrav2)
                    call wkvect('&&FLUIMP.TRAV3', 'V V K80', nzone, jtrav3)
                    call wkvect('&&FLUIMP.TRAV4', 'V V K80', nzone, jtrav4)
!
                    call jeveuo('&&COEFMO.VRZO', 'L', jvrzo)
!
                    do 15 j = 1, nzone
                        n1 = zi(jzone+2*(j-1)+1)
                        n2 = zi(jzone+2*(j-1)+2)
!                CONVERSION EN CHAINES DE CARACTERES
                        call codent(j, 'G', numzo)
                        call codent(n2-n1+1, 'G', nbpzon)
!
                        vrmin = zr(jvrzo+2*(j-1)+0)
                        call codree(vrmin, 'E', xvmin)
                        if (vrmin .lt. 0.d0) then
                            xvmin = '-'//xvmin
                        else
                            xvmin = ' '//xvmin
                        endif
!
                        vrmax = zr(jvrzo+2*(j-1)+1)
                        call codree(vrmax, 'E', xvmax)
                        if (vrmax .lt. 0.d0) then
                            xvmax = '-'//xvmax
                        else
                            xvmax = ' '//xvmax
                        endif
!
                        if (iret .ne. 0) then
                            vmoy = zr(jprofv+lnoe+n1)
                            call codree(vmoy, 'E', xvmoy)
                            if (vmoy .lt. 0.d0) then
                                xvmoy = '-'//xvmoy
                            else
                                xvmoy = ' '//xvmoy
                            endif
                        else
                            xvmoy = '      -      '
                        endif
!
                        zk80(jtrav1+j-1) = chazv2(1:8)//numzo(1:2)// '          *'
                        zk80(jtrav2+j-1) = chazp3(1:15)//xvmoy//'*'
                        zk80(jtrav3+j-1) = chazp5(1:23)//nbpzon(1:4)// ' *'
                        zk80(jtrav4+j-1) = chazv6(1:2 )//xvmin//' | ' //xvmax//' *'
15                  continue
!
                    write(ifr,3001) ' *',chav11,chav11,chav11,chav11,&
                    (chazp1,chazv1,j=1,nzone)
                    write(ifr,3001) ' *',chav12,chav12,chav12,chav13,&
                    (chazp2,zk80(jtrav1+j-1),j=1,nzone)
                    write(ifr,3001) ' *',chav11,chav11,chav11,chav11,&
                    (chazp1,chazv1,j=1,nzone)
                    write(ifr,3001) ' *',chav13,chav13,chav13,chav13,&
                    (zk80(jtrav2+j-1),chazv3,j=1,nzone)
                    write(ifr,3001) ' *',chav21,chav21,chav22,chav23,&
                    (chazp4,chazv4,j=1,nzone)
                    write(ifr,3001) ' *',chav31,chav32,chav33,chav34,&
                    (zk80(jtrav3+j-1),chazv5,j=1,nzone)
                    write(ifr,3001) ' *',chav13,chav13,chav13,chav13,&
                    (chazp6,zk80(jtrav4+j-1),j=1,nzone)
                    write(ifr,3001) ' *',chav13,chav13,chav13,chav13,&
                    (chazp7,chazv7,j=1,nzone)
                    write(ifr,3001) ' *',chav11,chav11,chav11,chav11,&
                    (chazp1,chazv1,j=1,nzone)
!
                    call jedetr('&&FLUIMP.TRAV1')
                    call jedetr('&&FLUIMP.TRAV2')
                    call jedetr('&&FLUIMP.TRAV3')
                    call jedetr('&&FLUIMP.TRAV4')
                endif
            else
                write(ifr,2001) ' *',chav11,chav11,chav11,chav11
                write(ifr,2001) ' *',chav12,chav12,chav12,chav13
                write(ifr,2001) ' *',chav11,chav11,chav11,chav11
                write(ifr,2001) ' *',chav13,chav13,chav13,chav13
                write(ifr,2001) ' *',chav21,chav21,chav22,chav23
                write(ifr,2001) ' *',chav31,chav32,chav33,chav34
                write(ifr,2001) ' *',chav13,chav13,chav13,chav13
                write(ifr,2001) ' *',chav13,chav13,chav13,chav13
                write(ifr,2001) ' *',chav11,chav11,chav11,chav11
            endif
!
            if (calcul(1)) then
! ---     ECRITURE DES LIGNES POUR CHAQUE VITESSE GAP
                do 20 iv = 1, npv
                    ind = 2*nbm*(iv-1) + 2*(im-1) + 1
                    freq1 = freq(ind)
                    amor1 = freq(ind+1)
                    dif1 = 1.d0-dble(abs(amor1))
!
                    if (vite(iv) .ge. 0) then
                        call codree(vite(iv), 'E', xcod)
                        xcod=' '//xcod
                    else
                        call codree(abs(vite(iv)), 'E', xcod)
                        xcod='-'//xcod
                    endif
!
                    if (freq1 .lt. 0.d0) then
                        chav40 = ' * '//xcod//' *            '// 'PROBLEME DE CONVERGENCE *'
                        if (itypfl .eq. 1) then
                            chaz40 = '                            |'// '        | *'
                            write(ifr,3002) chav40,(chaz40,j=1,nzone)
                        else
                            write(ifr,2002) chav40
                        endif
!
                    else if (dif1.lt.1.d-8) then
                        chav40 = ' * '//xcod//' *              '// 'SYSTEME SUR-AMORTI *'
                        if (itypfl .eq. 1) then
                            chaz40 = '                            |'// '        | *'
                            write(ifr,3002) chav40,(chaz40,j=1,nzone)
                        else
                            write(ifr,2002) chav40
                        endif
!
                    else
                        vred = vite(iv)/(freq1*carac(1))
                        if (vred .ge. 0) then
                            call codree(vred, 'E', xvred)
                            xvred = ' '//xvred
                        else
                            call codree(abs(vred), 'E', xvred)
                            xvred = '-'//xvred
                        endif
!
                        if (freq1 .ge. 0) then
                            call codree(freq1, 'E', xfreq1)
                            xfreq1 = ' '//xfreq1
                        else
                            call codree(abs(freq1), 'E', xfreq1)
                            xfreq1 = '-'//xfreq1
                        endif
!
                        if (amor1 .ge. 0) then
                            call codree(amor1*1.d+02, 'E', xamor)
                            xamor = ' '//xamor
                        else
                            call codree(abs(amor1*1.d+02), 'E', xamor)
                            xamor = '-'//xamor
                        endif
!
                        chav40 =' * '//xcod//' * '//xvred//' * '//&
                        xfreq1//' * '//xamor//' *'
!
                        if (itypfl .eq. 1) then
                            call wkvect('&&FLUIMP.TRAV5', 'V V K80', 10, jtrav5)
                            call jeveuo('&&PACOUC.TRAV1', 'L', jtr1)
                            call jeveuo('&&PACOUC.TRAV2', 'L', jtr2)
!
                            do 25 ik = 1, nzone
                                l1 = zi( jtr2 + 3*nzone*npv*(im-1)+ 3*nzone*(iv-1) + 3*(ik-1) )
                                l2 = zi(jtr2 + 3*nzone*npv*(im-1)+ 3*nzone*(iv-1) + 3*(ik-1) + 1)
                                l3 = zi(jtr2 + 3*nzone*npv*(im-1)+ 3*nzone*(iv-1) + 3*(ik-1) + 2)
                                bmin = zr(jtr1 + 2*nzone*npv*(im-1) + 2*nzone*(iv-1) + 2*(ik-1))
                                bmax = zr(&
                                       jtr1 +2*nzone*npv*(im-1) + 2*nzone*(iv-1) + 2*(ik-1) + 1)
                                call codent(l1, 'D', xl1)
                                call codent(l2, 'D', xl2)
                                call codent(l3, 'D', xl3)
                                if (l1 .eq. 0) then
                                    xbmin = '     -       '
                                else
                                    if (bmin .lt. 0.d0) then
                                        call codree(bmin, 'E', xbmin)
                                        xbmin = '-'//xbmin
                                    else
                                        call codree(bmin, 'E', xbmin)
                                        xbmin = ' '//xbmin
                                    endif
                                endif
                                if (l3 .eq. 0) then
                                    xbmax = '     -       '
                                else
                                    if (bmax .lt. 0.d0) then
                                        call codree(bmax, 'E', xbmax)
                                        xbmax = '-'//xbmax
                                    else
                                        call codree(bmax, 'E', xbmax)
                                        xbmax = ' '//xbmax
                                    endif
                                endif
                                zk80(jtrav5+ik-1) =' '//xl1(1:8)//' '//xl2(1:8)&
                                    &//' '//xl3(1:8)//' | '//xbmin//' | '&
                                    &//xbmax//' *'
25                          continue
                            write (ifr,3002) chav40,(zk80(jtrav5+j-1),&
                            j=1,nzone)
                            call jedetr('&&FLUIMP.TRAV5')
                        else
                            write (ifr,2002) chav40
                        endif
                    endif
20              continue
!
                if (itypfl .eq. 1) then
                    write(ifr,3001) '*',chav13,chav13,chav13,chav13,&
                    (chazp2,chazv7,j=1,nzone)
                    write(ifr,3001) '*',chav11,chav11,chav11,chav11,&
                    (chazp1,chazv1,j=1,nzone)
                else
                    write(ifr,2001) '*',chav13,chav13,chav13,chav13
                    write(ifr,2001) '*',chav11,chav11,chav11,chav11
                endif
            endif
            if (calcul(2)) then
!
                write (ifr,*) '==============================================='
                write (ifr,*)
                write (ifr,*) 'VALEURS DES VITESSES CRITIQUES ET DES RAPPORTS'
                write (ifr,*) '   D INSTABILITE PAR LA METHODE DE CONNORS'
                write (ifr,*)
                write (ifr,*) '==============================================='
                write (ifr,*)
                write (ifr,1003) zr(jconn)
                write (ifr,1004) zr(jconn+1)
!
                if (calcul(1)) then
!
                    write (ifr,5007)('*',j=1,117)
                    write (ifr,'(A)')' *   MODE    *      FREQUENCE(Hz)      *'//&
     &'    AMORTISSEMENT (%)    *  VITESSE EFFICACE (m/s) *'//&
     &'  VITESSE EFFICACE (m/s) *'
                    write (ifr,'(A)')' *           *                         *'//&
     &'                         *       (GEVIBUS)         *'//&
     &'       TTES CMPS         *'
!
                    write (ifr,5007)('*',j=1,117)
                    write (ifr,5001) imod,freqi(imod),(amoc(im)*100),&
                    zr(jven-1+im),zr(jven-1+nbm+im)
                    write (ifr,5007)('*',j=1,117)
                    write (ifr,*)
!
                endif
                write(ifr,*)
                write(ifr,*) '============================================'
                write(ifr,*)'PLAGE DE VARIATION DES CONSTANTES DE CONNORS'
                write(ifr,*) '============================================'
                write(ifr,*)
                write(ifr,5005) (cham30,j=1,nzone)
                write(ifr,5003) ('ZONE',i,i=1,nzone)
                write(ifr,5005) (cham30,j=1,nzone)
                write(ifr,5004) (zr(lfsvr+3+2*(j-1)), zr(lfsvr+3+2*(j-&
                1)+1),j=1,nzone)
                write(ifr,5005) (cham30,j=1,nzone)
                write(ifr,*)
                ctrav1=' *'
                ctrav3=' * '
                do 90 i = 1, nzone
                    call codent(i, 'D', i3)
                    ctrav1((3+30*(i-1)):(3+(30*i)))='           ZONE '//i3&
     &           //'          *'
90              continue
!             CTRAV1((3+(30*NZONE)+120):(3+(30*NZONE)+120))='*'
                ctrav3(((15*nzone)-9):((15*nzone)+12))=&
     &             ' CONSTANTES DE CONNORS'
                ctrav3((2+(30*nzone)):(2+(30*nzone)))='*'
                ctrav3((3+(30*nzone)):(3+(30*nzone)+120))=&
     & '    VITESSE CRITIQUE (m/s)   *   VITESSE REDUITE CRITIQUE  *&
     &    RAPPORT D INSTABILITE    *     RAPPORT TTES CMPS       *'
                write(ifr,'(A)') ctrav2(1:(2+(30*nzone)))
                write(ifr,'(A)') ctrav1(1:(2+(30*nzone)+90))
                write(ifr,'(A)') ctrav2(1:(2+(30*nzone)+120))
                write(ifr,'(A)') ctrav3(1:(2+(30*nzone)+120))
                write(ifr,'(A)') ctrav2(1:(2+(30*nzone)+120))
!
                do 100 i = 1, nbval
                    ctrav1='* '
                    do 110 j = 1, nzone
                        modul=1
                        do 120 k = (j+1), nzone
                            modul=modul*zi(lfsvi+1+nzone+k)
120                      continue
                        if (j .eq. 1) then
                            pas=(i-1)/modul
                        else
                            modul2=modul*zi(lfsvi+1+nzone+j)
                            pas=(mod((i-1),modul2))/modul
                        endif
                        zr(jcste-1+j)=zr(lfsvr+3+2*(j-1))+pas*&
                        (zr(lfsvr+3+2*(j-1)+1)-zr(lfsvr+3+2*(j-1)))&
                        /(zi(lfsvi+1+nzone+j)-1)
                        call codree(zr(jcste-1+j), 'E', ccste)
                        ctrav1((3+(30*(j-1))):(3+(30*j)))='  '//ccste//'  *'
!
110                  continue
                    reduit=zr(jvcn-1+(im-1)*nbval+i)/(freqi(imod)*&
                    carac(1))
                    rappor=zr(jrap-1+(im-1)*nbval+i)
                    rappo2=zr(jrap-1+nbm*nbval+(im-1)*nbval+i)
!
                    write(ifr,5006) zr(jcste), zr(jvcn-1+(im-1)*nbval+&
                    i), reduit,rappor,rappo2
!
100              continue
                write(ifr,'(A)') ctrav2(1:(2+(30*nzone)+120))
                write(ifr,*)
            endif
!
10      continue
!
    endif
!
    if (nivdef .eq. 1) then
!
        write (ifr,*)
        write (ifr,*) '==============================================='
        write (ifr,*)
        write (ifr,*) ' RESULTAT MODULE COUPLAGE FLUIDE-STRUCTURE'
        write (ifr,*)
        write (ifr,*) '        EVOLUTION DES DEFORMEES MODALES'
        write (ifr,*) '   EN FONCTION DE LA VITESSE DE L ECOULEMENT'
        write (ifr,*)
        write (ifr,*) '==============================================='
        write (ifr,*)
!
        if (itypfl .ne. 3) then
            npasv = 1
            write(ifr,*) 'LES DEFORMEES SOUS ECOULEMENT SONT INCHANGEES'//&
     &                 ' PAR RAPPORT A CELLES EN FLUIDE AU REPOS.'
            write(ifr,*)
        endif
!
        do 30 im = 1, nbm
            write(ifr,4001) nuor(im)
            write(ifr,*)
            write(cham19(14:16),'(I3.3)') nuor(im)
            do 40 iv = 1, npasv
                write(ifr,4002) iv
                write(ifr,*)
                write(cham19(17:19),'(I3.3)') iv
                call irdepl(cham19, ' ', ifr, 'RESULTAT', k8bid,&
                            k8bid, nomsym, ibid, lcor, 0,&
                            [0], 6, nomcmp, lsup, rbid,&
                            linf, rbid, lmax, lmin, lresu,&
                            formar, nive)
                write(ifr,*)
40          continue
30      continue
!
    endif
    call jedetr('&&FLUIMP.CSTE')
    call jedema()
! --- FORMATS
!
    1001 format (1p,' VITESSE MOYENNE SUR L ENSEMBLE DES ZONES = ',d13.6)
    1002 format (1p,' MODE : NUMERO D ORDRE:',i3,'/ FREQ:',d13.6)
    1003 format (1p,' MASSE LINEIQUE DE REFERENCE DU TUBE (kg/m) : ',e13.6)
    1004 format (1p,' MASSE VOLUMIQUE DE REFERENCE DU FLUIDE&
     & SECONDAIRE (kg/m3) : ',e13.6)
    2001 format (a2,4a16)
    2002 format (a66)
!
    3001 format (a2,4a16,30(a28,a33))
    3002 format (a66,30a61)
!
    4001 format (1x,' MODE N ',i3)
    4002 format (1x,' VITESSE N ',i3)
!
    5001 format (1p,1x,'*',3x,i3,5x,'*',4(5x,e13.6,7x,'*'))
    5003 format (1x,'*',100(10x,a4,1x,i3,11x,'*'))
    5004 format (1p,1x,100('*',d13.6,1x,'-',d13.6,1x))
    5005 format (1x,'*',100a30)
    5006 format (1p,1x,'*',5(8x,e13.6,8x,'*'))
    5007 format (1p,1x,118a1)
!
!
end subroutine

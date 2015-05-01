subroutine pjecou(ma1, ma2, nomgma, nomgno, corres)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.greffet at edf.fr
! ======================================================================
!     COMMANDE:  PROJ_CHAMP  METHODE:'COUPLAGE' (COUPLAGE IFS VIA YACS)
! ----------------------------------------------------------------------
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/elraca.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/pj3da3.h"
#include "asterfort/pj3da4.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: ma1, ma2
    character(len=16) :: nomgma, nomgno, corres
! ======================================================================
! ======================================================================
    integer :: iret, itypma, ndim, nbpg, ib, flag, ibt(20)
    integer :: nodegl, inol, ino2, mailrf, nbpgrf
    integer :: inog2, ii, ima1, ima, inom1, inom2, inom3
    integer :: nbmag1, nbnog2, nbnog
    integer :: ialim1, ialin2, jcoor1, icxma1
    integer :: iacono, iaconb, iacom1, iaconu, iacocf
    integer :: listno(27)
    real(kind=8) :: normgl, rbid, normlo
    real(kind=8) :: con1m2(3), con1m1(3), con2m1(3), con3m1(3)
    real(kind=8) :: cobary(3), ksi(2), ff(27), coefno(27), crrefe(81)
    character(len=8) :: ntypma, elref, cbt(15)
    character(len=24) :: grpma, grpno
    aster_logical :: inmail
    real(kind=8), pointer :: coor2(:) => null()
    integer, pointer :: typmail(:) => null()
! ======================================================================
!
!     MAILLES UTILES DU MAILLAGE 1
!     ============================
    grpma = ma1//'.GROUPEMA'
    call jeexin(jexnom(grpma, nomgma), iret)
    if (iret .eq. 0) then
        call utmess('F', 'ELEMENTS_62', sk=nomgma)
    endif
    call jelira(jexnom(grpma, nomgma), 'LONMAX', nbmag1)
    call jeveuo(jexnom(grpma, nomgma), 'L', ialim1)
!
!
!     NOEUDS UTILES DU MAILLAGE 2
!     ===========================
    grpno = ma2//'.GROUPENO'
    call jeexin(jexnom(grpno, nomgno), iret)
    if (iret .eq. 0) then
        call utmess('F', 'ELEMENTS_62', sk=nomgno)
    endif
    call jelira(jexnom(grpno, nomgno), 'LONMAX', nbnog2)
    call jeveuo(jexnom(grpno, nomgno), 'L', ialin2)
!
!
!     COORDONNEES DES NOEUDS DES MAILLAGES 1 ET 2
!     ===========================================
    call jeveuo(ma1//'.COORDO    .VALE', 'L', jcoor1)
    call jeveuo(ma2//'.COORDO    .VALE', 'L', vr=coor2)
!
!
!     CREATION D'UNE SD CORRESP_2_MAILLA
!     ==================================
! PJEF_NO : DEVIENT PJXX_K1 DEPUIS 10.1.9
!      CALL WKVECT(CORRES//'.PJEF_NO','V V K8',2,       IACONO)
    call wkvect(corres//'.PJXX_K1', 'V V K24', 5, iacono)
    zk24(iacono-1+3)='COUPLAGE'
    call wkvect(corres//'.PJEF_NB', 'V V I', nbnog2, iaconb)
    call wkvect(corres//'.PJEF_M1', 'V V I', nbnog2, iacom1)
    call wkvect(corres//'.PJEF_NU', 'V V I', 9*nbnog2, iaconu)
    call wkvect(corres//'.PJEF_CF', 'V V R', 9*nbnog2, iacocf)
!
!
!     AFFECTATION DES NOMS DES MAILLAGES
!     ==================================
    zk24(iacono-1+1)=ma1
    zk24(iacono-1+2)=ma2
!
!
!     CALCUL DES FONCTIONS DE FORMES DES NOEUDS DU MAILLAGE 2
!     =======================================================
    flag = 0
    do 10 inog2 = 1, nbnog2
!
        ino2 = zi(ialin2-1+inog2)
        do 20 ii = 1, 3
            con1m2(ii) = coor2(3*(ino2-1)+ii)
 20     continue
        mailrf = 0
!
!       RECHERCHE DES MAILLES1 ASSOCIEES AU NOEUD1
!       ==========================================
        normgl = 1.d20
        do 30 ima1 = 1, nbmag1
            ima = zi(ialim1-1+ima1)
!
!  ON RECUPERE LE NOM DU TYPE DE LA MAILLE
!  ET LE NOMBRE DE PTS D INTEGRATION ASSOCIE
!-----------------------------------------------------------------------
            call jelira(jexnum(ma1//'.CONNEX', ima), 'LONMAX', nbnog)
            call jeveuo(jexnum(ma1//'.CONNEX', ima), 'L', icxma1)
            call jeveuo(ma1//'.TYPMAIL', 'L', vi=typmail)
            itypma = typmail(ima)
            call jenuno(jexnum('&CATA.TM.NOMTM', itypma), ntypma)
            ii=5
            if (ntypma(1:3) .eq. 'SEG') ii=4
            elref(1:2)=ntypma(1:2)
            elref(3:3)=ntypma(ii:ii)
            elref(4:8)='     '
            call elraca(elref, ndim, nbpg, ib, ib,&
                        cbt, ibt, crrefe, rbid)
!
!         CAS OU LA MAILLE EST LINEIQUE (SEG)
!         -----------------------------------
            if (ntypma(1:3) .eq. 'SEG') then
                inom1 = zi(icxma1-1+1)
                inom2 = zi(icxma1-1+2)
                do 40 ii = 1, 3
                    con1m1(ii) = zr(jcoor1-1+3*(inom1-1)+ii)
                    con2m1(ii) = zr(jcoor1-1+3*(inom2-1)+ii)
 40             continue
! VERSION ORIGINALE
!            CALL PJ3D4C(CON1M2,CON1M1,CON2M1,INMAIL,COBARY,NORMLO)
                call pj3da4(con1m2, con1m1, con2m1, cobary(1), cobary(2),&
                            normlo)
                ksi(1) = 0
                do 50 ii = 1, 2
                    ksi(1) = ksi(1) + cobary(ii)*crrefe(ndim*(ii-1)+1)
 50             continue
!
!         CAS OU LA MAILLE EST SURFACIQUE (TRIA)
!         --------------------------------------
            else if (ntypma(1:4).eq.'TRIA') then
                inom1 = zi(icxma1-1+1)
                inom2 = zi(icxma1-1+2)
                inom3 = zi(icxma1-1+3)
                do 60 ii = 1, 3
                    con1m1(ii) = zr(jcoor1-1+3*(inom1-1)+ii)
                    con2m1(ii) = zr(jcoor1-1+3*(inom2-1)+ii)
                    con3m1(ii) = zr(jcoor1-1+3*(inom3-1)+ii)
 60             continue
!            CALL PJ3D3C(CON1M2,CON1M1,CON2M1,CON3M1,INMAIL,
!     &                  COBARY,NORMLO)
                call pj3da3(con1m2, con1m1, con2m1, con3m1, inmail,&
                            cobary(1), cobary(2), cobary(3), normlo)
                inmail = .true.
                ksi(1) = 0
                ksi(2) = 0
                do 70 ii = 1, 3
                    ksi(1) = ksi(1) + cobary(ii)*crrefe(ndim*(ii-1)+1)
                    ksi(2) = ksi(2) + cobary(ii)*crrefe(ndim*(ii-1)+2)
 70             continue
!
!         CAS OU LA MAILLE EST SURFACIQUE (QUAD)
!         --------------------------------------
            else if (ntypma(1:4).eq.'QUAD') then
!
!           On teste le premier triangle de l'element
!           -----------------------------------------
                inom1 = zi(icxma1-1+1)
                inom2 = zi(icxma1-1+2)
                inom3 = zi(icxma1-1+3)
                do 80 ii = 1, 3
                    con1m1(ii) = zr(jcoor1-1+3*(inom1-1)+ii)
                    con2m1(ii) = zr(jcoor1-1+3*(inom2-1)+ii)
                    con3m1(ii) = zr(jcoor1-1+3*(inom3-1)+ii)
 80             continue
!  VERSION ORIGINALE
!            CALL PJ3D3C(CON1M2,CON1M1,CON2M1,CON3M1,INMAIL,
!     &                  COBARY,NORMLO)
                call pj3da3(con1m2, con1m1, con2m1, con3m1, inmail,&
                            cobary(1), cobary(2), cobary(3), normlo)
!
!
!           On teste le second triangle de l'element
!           ----------------------------------------
                if (.not.(inmail)) then
                    inom2 = zi(icxma1-1+3)
                    inom3 = zi(icxma1-1+4)
                    do 90 ii = 1, 3
                        con2m1(ii) = zr(jcoor1-1+3*(inom2-1)+ii)
                        con3m1(ii) = zr(jcoor1-1+3*(inom3-1)+ii)
 90                 continue
!  VERSION ORIGINALE
!            CALL PJ3D3C(CON1M2,CON1M1,CON2M1,CON3M1,INMAIL,
!     &                  COBARY,NORMLO)
                    call pj3da3(con1m2, con1m1, con2m1, con3m1, inmail,&
                                cobary(1), cobary(2), cobary(3), normlo)
                    ksi(1) = cobary(1)*crrefe(1)
                    ksi(2) = cobary(1)*crrefe(2)
                    do 100 ii = 2, 3
                        ksi(1) = ksi(1) + cobary(ii)*crrefe(ndim*(ii)+ 1)
                        ksi(2) = ksi(2) + cobary(ii)*crrefe(ndim*(ii)+ 2)
100                 continue
                else
                    ksi(1) = 0.d0
                    ksi(2) = 0.d0
                    do 110 ii = 1, 3
                        ksi(1) = ksi(1) + cobary(ii)*crrefe(ndim*(ii- 1)+1)
                        ksi(2) = ksi(2) + cobary(ii)*crrefe(ndim*(ii- 1)+2)
110                 continue
                endif
            else
!            WRITE (6,*) 'TYPE DE MAILLE NON RECONNUE : ',NTYPMA
                call utmess('F', 'COUPLAGEIFS_9', sk=ntypma)
            endif
!
!   SI LE POINT EST DANS LA MAILLE
!   ET SI LA DISTANCE EST INFERIEURE A LA REFERENCE ALORS SAUVEGARDE
!-----------------------------------------------------------------------
            if (inmail .and. (normlo.lt.normgl)) then
                normgl = normlo
                call elrfvf(elref, ksi, 27, ff, nbpg)
                nbpgrf = nbpg
                mailrf = ima
                do 120 ii = 1, nbpgrf
                    listno(ii) = zi(icxma1-1+ii)
                    coefno(ii) = ff(ii)
120             continue
            endif
 30     continue
!
!       AFFECTATION DU NOEUD LE PLUS PROCHE EN CAS DE PROBLEME
!       ======================================================
        if (mailrf .eq. 0) then
            normgl = 1.d20
            nodegl = 0
            do 130 ima1 = 1, nbmag1
                ima = zi(ialim1-1+ima1)
                call jelira(jexnum(ma1//'.CONNEX', ima), 'LONMAX', nbnog)
                call jeveuo(jexnum(ma1//'.CONNEX', ima), 'L', icxma1)
                do 140 inom1 = 1, nbnog
                    inol = jcoor1-1+3*(zi(icxma1-1+inom1)-1)
                    normlo = 0.d0
                    do 150 ii = 1, 3
                        normlo = normlo + (zr(inol+ii)-con1m2(ii))**2
150                 continue
                    if (normlo .lt. normgl) then
                        normgl = normlo
                        nodegl = zi(icxma1-1+inom1)
                    endif
140             continue
130         continue
            mailrf = 0
            nbpgrf = 1
            listno(1) = nodegl
            coefno(1) = 1.d0
        endif
!
!       AFFECTATION DU NOEUD DANS LA STRUCTURE CORRES
!       =============================================
        zi(iaconb-1+inog2) = nbpgrf
        zi(iacom1-1+inog2) = mailrf
        do 160 ii = 1, nbpgrf
            zi(iaconu-1+flag+ii) = listno(ii)
            zr(iacocf-1+flag+ii) = coefno(ii)
160     continue
        flag = flag + nbpgrf
 10 end do
! ======================================================================
! IMPRESSIONS POUR VERIFICATION
!      FLAG = 0
!      PRINT*,'GROUPE DE NOEUDS : ',NOMGNO
!      DO INOG2 = 1, NBNOG2
!        PRINT*,'NUMERO DU NOEUD (L/G)   : ',INOG2,ZI(IALIN2-1+INOG2)
!        PRINT*,'COORDONNEES             : ',
!     &(ZR(JCOOR2-1+3*(ZI(IALIN2-1+INOG2)-1)+II),II=1,3)
!        PRINT*,'MAILLE DE REFERENCE     : ',ZI(IACOM1-1+INOG2)
!        PRINT*,'NOMBRE DE NOEUDS        : ',ZI(IACONB-1+INOG2)
!        PRINT*,'LISTE DES NOEUDS        : ',(ZI(IACONU-1+FLAG+II),
!     &                                       II=1,ZI(IACONB-1+INOG2))
!        PRINT*,'COEFFICIENTS DES NOEUDS : ',(ZR(IACOCF-1+FLAG+II),
!     &                                       II=1,ZI(IACONB-1+INOG2))
!        PRINT*
!        FLAG = FLAG + ZI(IACONB-1+INOG2)
!      ENDDO
! ======================================================================
end subroutine

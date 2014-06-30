subroutine te0539(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nmtstm.h"
#include "asterfort/rcangm.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/xnmel.h"
#include "asterfort/xnmgr.h"
#include "asterfort/xnmpl.h"
#include "asterfort/xteddl.h"
#include "asterfort/xteini.h"
#include "blas/dcopy.h"
    character(len=16) :: option, nomte
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!                          ELEMENTS 3D AVEC X-FEM
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    character(len=8) :: typmod(2), enr, lag
    character(len=16) :: compor(4), elref
!      CHARACTER*4 FAMI
    integer :: jgano, nno, npg, i, imatuu, lgpg, ndim, lgpg1, iret, nfiss
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: icontm, ivarim
    integer :: iinstm, iinstp, ideplm, ideplp, icompo, icarcr
    integer :: ivectu, icontp, ivarip, li, jcret, codret
    integer :: ivarix
    integer :: jpintt, jcnset, jheavt, jlonch, jbaslo, jlsn, jlst, jstno, jpmilt
    integer :: jtab(7), nnos, idim, jfisno
    integer :: nfh, ddlc, nddl, nnom, nfe, ibid, ddls, ddlm
    logical(kind=1) :: matsym
    logical :: lcond
    real(kind=8) :: angmas(7), bary(3), crit(1), sig(1), vi(1)
!
    ivectu=1
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    call elref1(elref)
!      FAMI='RIGI'
!     MATNS MAL DIMENSIONNEE
    ASSERT(nno.le.27)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, ibid, ddlc,&
                nnom, ddls, nddl, ddlm, nfiss,&
                ibid)
!
! - TYPE DE MODELISATION
    if (ndim .eq. 3) then
        typmod(1) = '3D'
        typmod(2) = ' '
    else
        if (lteatt('AXIS','OUI')) then
            typmod(1) = 'AXIS'
        else if (lteatt('C_PLAN','OUI')) then
            typmod(1) = 'C_PLAN'
        else if (lteatt('D_PLAN','OUI')) then
            typmod(1) = 'D_PLAN'
        else
!          NOM D'ELEMENT ILLICITE
            lcond=lteatt('C_PLAN', 'OUI')
            ASSERT(lcond)
        endif
        typmod(2) = ' '
        codret=0
    endif
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA' .or. option(1:10)&
        .eq. 'RIGI_MECA_') then
!
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PDEPLPR', 'L', ideplp)
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PCARCRI', 'L', icarcr)
        call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                    itab=jtab)
        lgpg1 = max(jtab(6),1)*jtab(7)
        lgpg = lgpg1
    endif
!     PARAMETRES PROPRES Á X-FEM
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASLOR', 'L', jbaslo)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PSTANO', 'L', jstno)
!     PROPRES AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
    call teattr('S', 'XFEM', enr, ibid)
    if ((ibid.eq.0) .and. (.not.lteatt('AXIS','OUI')) .and.&
        (enr.eq.'XH' .or.enr.eq.'XHT'.or.enr.eq.'XT'.or.enr.eq.'XHC')&
         .and..not.iselli(elref)) &
    call jevech('PPMILTO', 'L', jpmilt)
    if (nfiss .gt. 1) call jevech('PFISNO', 'L', jfisno)
!
!---- CALCUL POUR L'OPTION RIGI_MECA (APPEL DEPUIS MERIME)
    if (option .eq. 'RIGI_MECA') then
        call jevech('PMATUUR', 'E', imatuu)
        lgpg=0
        compor(1)=' '
        compor(2)=' '
        compor(3)=' '
        compor(4)=' '
        call xnmel('-', nno, nfh, nfe, ddlc,&
                   ddlm, igeom, typmod, option, zi( imate),&
                   compor, lgpg, crit, jpintt, zi(jcnset),&
                   zi(jheavt), zi( jlonch), zr(jbaslo), ibid, zr(jlsn),&
                   zr(jlst), sig, vi, zr(imatuu), ibid,&
                   codret, jpmilt, nfiss, jfisno)
!
!-------ON MET NE DUR LE FAIT QUE LA MATRICE EST SYMETRIQUE
        matsym=.true.
        goto 999
    else
        imatuu=1
    endif
!---------------------------------------------------------
!
! --- ORIENTATION DU MASSIF
!     COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
!
    bary(1) = 0.d0
    bary(2) = 0.d0
    bary(3) = 0.d0
    do i = 1, nno
        do idim = 1, ndim
            bary(idim) = bary(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
        end do
    end do
    call rcangm(ndim, bary, angmas)
!
! - VARIABLES DE COMMANDE
!
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
!
! - PARAMETRES EN SORTIE
!
    if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
        call nmtstm(zk16(icompo), imatuu, matsym)
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
!
! ATTENTION : ICONTM ET ICONTP : SIGMA AUX PTS DE GAUSS DES SOUS-TETRAS
!
!      ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
        call jevech('PVARIMP', 'L', ivarix)
        npg = jtab(2)
        call dcopy(npg*lgpg, zr(ivarix), 1, zr(ivarip), 1)
    else
        ivectu=1
        icontp=1
        ivarip=1
    endif
!
! provisoire. Mieux : utiliser ELAS_INCR
    if ((zk16(icompo).eq. 'ELAS') .and. (zk16(icompo+2).eq.'GROT_GDEP')) then
        zk16(icompo+3)='COMP_INCR'
    endif
!
    if (zk16(icompo+3) (1:9) .eq. 'COMP_ELAS') then
!
! - LOIS DE COMPORTEMENT ECRITES EN CONFIGURATION DE REFERENCE
!                          COMP_ELAS
!
        if (option(1:10) .eq. 'RIGI_MECA_') then
!
!         OPTION RIGI_MECA_TANG :         ARGUMENTS EN T-
            call xnmel('-', nno, nfh, nfe, ddlc,&
                       ddlm, igeom, typmod, option, zi(imate),&
                       zk16(icompo), lgpg, zr(icarcr), jpintt, zi(jcnset),&
                       zi(jheavt), zi(jlonch), zr(jbaslo), ideplm, zr(jlsn),&
                       zr(jlst), zr(icontm), zr(ivarim), zr(imatuu), ivectu,&
                       codret, jpmilt, nfiss, jfisno)
        else
!
!        OPTION FULL_MECA OU RAPH_MECA : ARGUMENTS EN T+
            do li = 1, nddl
                zr(ideplp+li-1) = zr(ideplm+li-1) + zr(ideplp+li-1)
            end do
!
            call xnmel('+', nno, nfh, nfe, ddlc,&
                       ddlm, igeom, typmod, option, zi(imate),&
                       zk16(icompo), lgpg, zr(icarcr), jpintt, zi(jcnset),&
                       zi(jheavt), zi(jlonch), zr(jbaslo), ideplp, zr(jlsn),&
                       zr(jlst), zr(icontp), zr(ivarip), zr(imatuu), ivectu,&
                       codret, jpmilt, nfiss, jfisno)
        endif
!
    else
!
! - LOIS DE COMPORTEMENT ECRITE EN CONFIGURATION ACTUELLE
!                          COMP_INCR
!
!      PETITES DEFORMATIONS (AVEC EVENTUELLEMENT REACTUALISATION)
        if (zk16(icompo+2) (1:5) .eq. 'PETIT') then
            if (zk16(icompo+2) (6:10) .eq. '_REAC') then
                call utmess('F', 'XFEM_50')
!            DO 20 I = 1,3*NNO
! --- ATTENTION, UTILISER PETIT_REAC EST FAUX CAR IL FAUT AUSSI
! --- REACTUALISER LA GEOMETRIE DES POINTS D'INTERSECTION ZR(JPINTT)
!              ZR(IGEOM+I-1) = ZR(IGEOM+I-1) + ZR(IDEPLM+I-1) +
!     &                        ZR(IDEPLP+I-1)
!   20       CONTINUE
            endif
!
            call xnmpl(nno, nfh, nfe, ddlc, ddlm,&
                       igeom, zr(iinstm), zr( iinstp), ideplp, zr(icontm),&
                       zr(ivarip), typmod, option, zi( imate), zk16(icompo),&
                       lgpg, zr(icarcr), jpintt, zi(jcnset), zi(jheavt),&
                       zi(jlonch), zr(jbaslo), ideplm, zr(jlsn), zr(jlst),&
                       zr(icontp), zr(ivarim), zr(imatuu), ivectu, codret,&
                       jpmilt, nfiss, jfisno)
!
! 7.3 - GRANDES ROTATIONS ET PETITES DEFORMATIONS
        else if (zk16(icompo+2).eq.'GROT_GDEP') then
!            DO 50 I = 1,3*NNO
            do i = 1, nddl
                zr(ideplp+i-1) = zr(ideplm+i-1) + zr(ideplp+i-1)
            end do
!
            call xnmgr(nno, nfh, nfe, ddlc, ddlm,&
                       igeom, zr(iinstm), zr( iinstp), ideplp, zr(icontm),&
                       zr(ivarip), typmod, option, zi( imate), zk16(icompo),&
                       lgpg, zr(icarcr), jpintt, zi(jcnset), zi(jheavt),&
                       zi(jlonch), zr(jbaslo), ideplm, zr(jlsn), zr(jlst),&
                       nfiss, jfisno, zr(icontp), zr(ivarim), zr(imatuu),&
                       ivectu, codret, jpmilt)
!
        else
            call utmess('F', 'ELEMENTS3_16', sk=zk16(icompo+2))
        endif
!
!       ELSE
!
!        CALL UTMESS('F','ELEMENTS4_23')
!
!
! PARTIE 2D
! - HYPO-ELASTICITE
!
!         IF (ZK16(ICOMPO+2) (6:10).EQ.'_REAC') THEN
! CCDIR$ IVDEP
!           DO 25 I = 1,2*NNO
!             ZR(IGEOM+I-1) = ZR(IGEOM+I-1) + ZR(IDEPLM+I-1) +
!      &                      ZR(IDEPLP+I-1)
!   25     CONTINUE
!         ENDIF
!
!         IF (ZK16(ICOMPO+2) (1:5).EQ.'PETIT') THEN
!
! C -       ELEMENT A DISCONTINUITE INTERNE
!           IF (TYPMOD(2).EQ.'ELEMDISC') THEN
!
!             CALL NMED2D(NNO,NPG,IPOIDS,IVF,IDFDE,
!      &              ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),ZK16(ICOMPO),
!      &              LGPG,ZR(ICARCR),
!      &              ZR(IDEPLM),ZR(IDEPLP),
!      &              ZR(ICONTM),ZR(IVARIM),VECT1,
!      &              VECT3,ZR(ICONTP),ZR(IVARIP),
!      &              ZR(IMATUU),ZR(IVECTU),CODRET)
!
!           ELSE
!
!             CALL NMPL2D(FAMI,NNO,NPG,IPOIDS,IVF,IDFDE,
!      &              ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),ZK16(ICOMPO),
!      &              LGPG,ZR(ICARCR),
!      &              ZR(IINSTM),ZR(IINSTP),
!      &              ZR(IDEPLM),ZR(IDEPLP),ANGMAS,
!      &              ZR(ICONTM),ZR(IVARIM),MATSYM,VECT1,
!      &              VECT3,ZR(ICONTP),ZR(IVARIP),
!      &              ZR(IMATUU),ZR(IVECTU),CODRET)
!
!           ENDIF
!
    endif
!
999 continue
!
!     SUPPRESSION DES DDLS SUPERFLUS
    call teattr('C', 'XLAG', lag, ibid)
    if (ibid .eq. 0 .and. lag .eq. 'ARETE') then
        nno = nnos
    endif
!
!   OPTIONS RELATIVES A UNE MATRICE UNIQUEMENT
    if     (option .eq. 'RIGI_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
        call xteddl(ndim, nfh, nfe, ddls, nddl,&
                    nno, nnos, zi(jstno), .false._1, matsym,&
                    option, nomte, ddlm, nfiss, jfisno,&
                    mat=zr(imatuu))
!   OPTIONS RELATIVES A UN VECTEUR UNIQUEMENT
    elseif (option .eq. 'RAPH_MECA') then
        call xteddl(ndim, nfh, nfe, ddls, nddl,&
                    nno, nnos, zi(jstno), .false._1, matsym,&
                    option, nomte, ddlm, nfiss, jfisno,&
                    vect=zr(ivectu))
!   OPTIONS RELATIVES A UNE MATRICE ET UN VECTEUR
    elseif (option .eq. 'FULL_MECA') then
        call xteddl(ndim, nfh, nfe, ddls, nddl,&
                    nno, nnos, zi(jstno), .false._1, matsym,&
                    option, nomte, ddlm, nfiss, jfisno,&
                    mat=zr(imatuu), vect=zr(ivectu))
    else
        ASSERT(.false.)
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
!
end subroutine

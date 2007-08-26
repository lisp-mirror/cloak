#include <jni.h>
#include <stdarg.h>

#define va_getter(name, type)			\
	type					\
	va_##name(va_list *ptr)		\
	{					\
		va_list ap = *ptr;		\
		type result = va_arg(ap, type);	\
		*ptr = ap;			\
		return result;			\
	}

va_getter(int, int)
va_getter(longlong, long long)
va_getter(double, double)

static jobject
NewObject_tramp(JNIEnv *env, jclass class, jmethodID method, ...)
{
	va_list ap;
	jobject result;

	va_start(ap, method);
	result = (*env)->NewObjectV(env, class, method, ap);
	va_end(ap);

	return result;
}

static void
CallVoidMethod_tramp(JNIEnv *env, jobject obj, jmethodID method, ...)
{
	va_list ap;
	va_start(ap, method);
	(*env)->CallVoidMethodV(env, obj, method, ap);
	va_end(ap);
}

static void
CallStaticVoidMethod_tramp(JNIEnv *env, jclass class, jmethodID method, ...)
{
	va_list ap;
	va_start(ap, method);
	(*env)->CallStaticVoidMethodV(env, class, method, ap);
	va_end(ap);
}

static void
CallNonvirtualVoidMethod_tramp(
	JNIEnv *env, jobject obj, jclass class, jmethodID method, ...)
{
	va_list ap;
	va_start(ap, method);
	(*env)->CallNonvirtualVoidMethodV(env, obj, class, method, ap);
	va_end(ap);
}

#define virtual_tramp(name, type)					\
	static type							\
	name##_tramp(JNIEnv *env, jobject obj, jmethodID method, ...)	\
	{								\
		type result;						\
		va_list ap;						\
		va_start(ap, method);					\
		result = (*env)->name##V(env, obj, method, ap);		\
		va_end(ap);						\
		return result;						\
	}

#define static_tramp(name, type)					\
	static type							\
	name##_tramp(JNIEnv *env, jclass class, jmethodID method, ...)	\
	{								\
		type result;						\
		va_list ap;						\
		va_start(ap, method);					\
		result = (*env)->name##V(env, class, method, ap);	\
		va_end(ap);						\
		return result;						\
	}

#define nonvirtual_tramp(name, type)					\
	static type							\
	name##_tramp(							\
                JNIEnv *env, jobject obj, jclass cls, jmethodID method, ...) \
	{								\
		type result;						\
		va_list ap;						\
		va_start(ap, method);					\
		result = (*env)->name##V(env, obj, cls, method, ap);	\
		va_end(ap);						\
		return result;						\
	}

virtual_tramp(CallObjectMethod, jobject)
virtual_tramp(CallBooleanMethod, jboolean)
virtual_tramp(CallByteMethod, jbyte)
virtual_tramp(CallCharMethod, jchar)
virtual_tramp(CallShortMethod, jshort)
virtual_tramp(CallIntMethod, jint)
virtual_tramp(CallLongMethod, jlong)
virtual_tramp(CallFloatMethod, jfloat)
virtual_tramp(CallDoubleMethod, jdouble)

static_tramp(CallStaticObjectMethod, jobject)
static_tramp(CallStaticBooleanMethod, jboolean)
static_tramp(CallStaticByteMethod, jbyte)
static_tramp(CallStaticCharMethod, jchar)
static_tramp(CallStaticShortMethod, jshort)
static_tramp(CallStaticIntMethod, jint)
static_tramp(CallStaticLongMethod, jlong)
static_tramp(CallStaticFloatMethod, jfloat)
static_tramp(CallStaticDoubleMethod, jdouble)

nonvirtual_tramp(CallNonvirtualObjectMethod, jobject)
nonvirtual_tramp(CallNonvirtualBooleanMethod, jboolean)
nonvirtual_tramp(CallNonvirtualByteMethod, jbyte)
nonvirtual_tramp(CallNonvirtualCharMethod, jchar)
nonvirtual_tramp(CallNonvirtualShortMethod, jshort)
nonvirtual_tramp(CallNonvirtualIntMethod, jint)
nonvirtual_tramp(CallNonvirtualLongMethod, jlong)
nonvirtual_tramp(CallNonvirtualFloatMethod, jfloat)
nonvirtual_tramp(CallNonvirtualDoubleMethod, jdouble)

void
install_trampolines(struct JNINativeInterface **env)
{
	(*env)->NewObject = NewObject_tramp;

	(*env)->CallVoidMethod = CallVoidMethod_tramp;
	(*env)->CallObjectMethod = CallObjectMethod_tramp;
	(*env)->CallBooleanMethod = CallBooleanMethod_tramp;
	(*env)->CallByteMethod = CallByteMethod_tramp;
	(*env)->CallCharMethod = CallCharMethod_tramp;
	(*env)->CallShortMethod = CallShortMethod_tramp;
	(*env)->CallIntMethod = CallIntMethod_tramp;
	(*env)->CallLongMethod = CallLongMethod_tramp;
	(*env)->CallFloatMethod = CallFloatMethod_tramp;
	(*env)->CallDoubleMethod = CallDoubleMethod_tramp;

	(*env)->CallStaticVoidMethod = CallStaticVoidMethod_tramp;
	(*env)->CallStaticObjectMethod = CallStaticObjectMethod_tramp;
	(*env)->CallStaticBooleanMethod = CallStaticBooleanMethod_tramp;
	(*env)->CallStaticByteMethod = CallStaticByteMethod_tramp;
	(*env)->CallStaticCharMethod = CallStaticCharMethod_tramp;
	(*env)->CallStaticShortMethod = CallStaticShortMethod_tramp;
	(*env)->CallStaticIntMethod = CallStaticIntMethod_tramp;
	(*env)->CallStaticLongMethod = CallStaticLongMethod_tramp;
	(*env)->CallStaticFloatMethod = CallStaticFloatMethod_tramp;
	(*env)->CallStaticDoubleMethod = CallStaticDoubleMethod_tramp;

	(*env)->CallNonvirtualVoidMethod = CallNonvirtualVoidMethod_tramp;
	(*env)->CallNonvirtualObjectMethod = CallNonvirtualObjectMethod_tramp;
	(*env)->CallNonvirtualBooleanMethod = CallNonvirtualBooleanMethod_tramp;
	(*env)->CallNonvirtualByteMethod = CallNonvirtualByteMethod_tramp;
	(*env)->CallNonvirtualCharMethod = CallNonvirtualCharMethod_tramp;
	(*env)->CallNonvirtualShortMethod = CallNonvirtualShortMethod_tramp;
	(*env)->CallNonvirtualIntMethod = CallNonvirtualIntMethod_tramp;
	(*env)->CallNonvirtualLongMethod = CallNonvirtualLongMethod_tramp;
	(*env)->CallNonvirtualFloatMethod = CallNonvirtualFloatMethod_tramp;
	(*env)->CallNonvirtualDoubleMethod = CallNonvirtualDoubleMethod_tramp;
}
